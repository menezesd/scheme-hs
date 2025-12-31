{-# LANGUAGE FlexibleContexts #-}

-- | Scheme Primitives
--
-- This module provides all primitive functions for the R5RS Scheme interpreter.
-- It re-exports primitives from specialized sub-modules:
--
-- * "Primitives.Numeric" - Arithmetic, comparison, numeric tower operations
-- * "Primitives.String" - String manipulation and conversion
-- * "Primitives.Char" - Character predicates and conversion
-- * "Primitives.List" - List operations and equivalence predicates
-- * "Primitives.Vector" - Vector operations
-- * "Primitives.IO" - I/O operations, ports, and mutable pairs
--
-- == Usage
--
-- The main entry point is 'primitiveBindings', which creates an environment
-- with all primitive functions bound:
--
-- @
-- env <- primitiveBindings
-- -- env now contains all primitive functions
-- @
module Primitives
    ( -- * Environment initialization
      primitiveBindings
      -- * Primitive collections
    , primitives
    , ioPrimitives
      -- * Re-exports from sub-modules
      -- ** Numeric
    , module Primitives.Numeric
      -- ** String
    , module Primitives.String
      -- ** Character
    , module Primitives.Char
      -- ** List
    , module Primitives.List
      -- ** Vector
    , module Primitives.Vector
      -- ** I/O
    , module Primitives.IO
    ) where

import Types
import Env
import Control.Monad.Except

-- Sub-modules
import Primitives.Numeric
import Primitives.String
import Primitives.Char
import Primitives.List
import Primitives.Vector
import Primitives.IO hiding (ioPrimitives)
import qualified Primitives.IO as PIO

-- | Create environment with all primitives bound
--
-- This function creates a new environment and binds all primitive functions
-- (both pure and I/O) to their names. This is typically called once at
-- interpreter startup.
--
-- ==== Example
--
-- @
-- main :: IO ()
-- main = do
--     env <- primitiveBindings
--     -- Use env to evaluate expressions
-- @
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives
                                            ++ map makeIOFunc ioPrimitives)
  where
    makePrimitiveFunc (name, func) = (name, PrimitiveFunc func)
    makeIOFunc (name, func) = (name, IOFunc func)

-- | All pure primitive functions
--
-- This list contains all non-I/O primitive functions, organized by category:
--
-- * Numeric operations (arithmetic, comparison, type predicates)
-- * Boolean operations
-- * Type predicates
-- * Symbol operations
-- * String operations
-- * Character operations
-- * List operations
-- * Vector operations
-- * Equivalence predicates
-- * Error handling
-- * Multiple values
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = concat
    [ numericPrimitives
    , stringPrimitives
    , charPrimitives
    , listPrimitives
    , vectorPrimitives
    , booleanPrimitives
    , typePrimitives
    , symbolPrimitives
    , portPredicates
    , miscPrimitives
    ]

-- | Boolean operations
booleanPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
booleanPrimitives =
    [ ("not", notOp)
    , ("boolean?", isBoolP)
    ]

-- | Boolean not
notOp :: [LispVal] -> ThrowsError LispVal
notOp [Bool False] = return $ Bool True
notOp [_] = return $ Bool False
notOp args = throwError $ NumArgs 1 args

-- | Check if value is boolean
isBoolP :: [LispVal] -> ThrowsError LispVal
isBoolP [Bool _] = return $ Bool True
isBoolP [_] = return $ Bool False
isBoolP args = throwError $ NumArgs 1 args

-- | Type predicates
typePrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
typePrimitives =
    [ ("symbol?", isSymbolP)
    , ("string?", isStringP)
    , ("char?", isCharP)
    , ("pair?", isPairP)
    , ("list?", isListP)
    , ("null?", isNullP)
    , ("vector?", isVectorP)
    , ("procedure?", isProcedureP)
    ]

-- | Check if value is a symbol
isSymbolP :: [LispVal] -> ThrowsError LispVal
isSymbolP [Atom _] = return $ Bool True
isSymbolP [_] = return $ Bool False
isSymbolP args = throwError $ NumArgs 1 args

-- | Check if value is a string
isStringP :: [LispVal] -> ThrowsError LispVal
isStringP [String _] = return $ Bool True
isStringP [_] = return $ Bool False
isStringP args = throwError $ NumArgs 1 args

-- | Check if value is a character
isCharP :: [LispVal] -> ThrowsError LispVal
isCharP [Char _] = return $ Bool True
isCharP [_] = return $ Bool False
isCharP args = throwError $ NumArgs 1 args

-- | Check if value is a pair (non-empty list or dotted list)
isPairP :: [LispVal] -> ThrowsError LispVal
isPairP [List (_:_)] = return $ Bool True
isPairP [DottedList _ _] = return $ Bool True
isPairP [_] = return $ Bool False
isPairP args = throwError $ NumArgs 1 args

-- | Check if value is a proper list
isListP :: [LispVal] -> ThrowsError LispVal
isListP [List _] = return $ Bool True
isListP [_] = return $ Bool False
isListP args = throwError $ NumArgs 1 args

-- | Check if value is the empty list
isNullP :: [LispVal] -> ThrowsError LispVal
isNullP [List []] = return $ Bool True
isNullP [_] = return $ Bool False
isNullP args = throwError $ NumArgs 1 args

-- | Check if value is a vector
isVectorP :: [LispVal] -> ThrowsError LispVal
isVectorP [Vector _] = return $ Bool True
isVectorP [_] = return $ Bool False
isVectorP args = throwError $ NumArgs 1 args

-- | Check if value is a procedure
isProcedureP :: [LispVal] -> ThrowsError LispVal
isProcedureP [PrimitiveFunc _] = return $ Bool True
isProcedureP [IOFunc _] = return $ Bool True
isProcedureP [Func {}] = return $ Bool True
isProcedureP [_] = return $ Bool False
isProcedureP args = throwError $ NumArgs 1 args

-- | Symbol operations
symbolPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
symbolPrimitives =
    [ ("symbol->string", symbolToString)
    , ("string->symbol", stringToSymbol)
    ]

-- | Convert symbol to string
symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString [notSym] = throwError $ TypeMismatch "symbol" notSym
symbolToString args = throwError $ NumArgs 1 args

-- | Convert string to symbol
stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return $ Atom s
stringToSymbol [notStr] = throwError $ TypeMismatch "string" notStr
stringToSymbol args = throwError $ NumArgs 1 args

-- | Port predicates (pure versions)
portPredicates :: [(String, [LispVal] -> ThrowsError LispVal)]
portPredicates =
    [ ("port?", unaryOp isPort)
    , ("input-port?", unaryOp isInputPort)
    , ("output-port?", unaryOp isOutputPort)
    , ("eof-object?", unaryOp isEOFObject)
    ]
  where
    unaryOp f [v] = f v
    unaryOp _ args = throwError $ NumArgs 1 args

-- | Miscellaneous primitives
miscPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
miscPrimitives =
    [ ("error", errorProc)
    , ("values", valuesProc)
    ]

-- | Raise an error
errorProc :: [LispVal] -> ThrowsError LispVal
errorProc [String msg] = throwError $ Default msg
errorProc [msg] = throwError $ Default $ show msg
errorProc args = throwError $ Default $ "Error: " ++ unwords (map show args)

-- | Return multiple values (simplified)
valuesProc :: [LispVal] -> ThrowsError LispVal
valuesProc [val] = return val
valuesProc vals = return $ DottedList [Atom "#values"] (List vals)

-- | All I/O primitive functions
--
-- These primitives perform I/O operations and must run in the 'IOThrowsError' monad.
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = PIO.ioPrimitives
