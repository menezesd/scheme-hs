{-# LANGUAGE FlexibleContexts #-}

-- | Character primitives for R5RS Scheme
--
-- This module implements character operations including:
--
-- * Character comparison (case-sensitive and case-insensitive)
-- * Character classification predicates
-- * Character case conversion
-- * Character/integer conversion
module Primitives.Char
    ( -- * Primitive bindings
      charPrimitives
      -- * Character comparison
    , charBoolBinop
    , charCiBoolBinop
      -- * Character predicates
    , charAlphabetic
    , charNumeric
    , charWhitespace
    , charUpperCase
    , charLowerCase
      -- * Character conversion
    , charToInteger
    , integerToChar
    , charUpcase
    , charDowncase
    ) where

import Types
import Control.Monad.Except
import Data.Char (toUpper, toLower, isAlpha, isDigit, isSpace, isUpper, isLower)

-- | All character primitive functions
charPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
charPrimitives =
    [ ("char=?", charBoolBinop (==))
    , ("char<?", charBoolBinop (<))
    , ("char>?", charBoolBinop (>))
    , ("char<=?", charBoolBinop (<=))
    , ("char>=?", charBoolBinop (>=))
    , ("char-ci=?", charCiBoolBinop (==))
    , ("char-ci<?", charCiBoolBinop (<))
    , ("char-ci>?", charCiBoolBinop (>))
    , ("char-ci<=?", charCiBoolBinop (<=))
    , ("char-ci>=?", charCiBoolBinop (>=))
    , ("char->integer", unaryOp charToInteger)
    , ("integer->char", unaryOp integerToChar)
    , ("char-upcase", unaryOp charUpcase)
    , ("char-downcase", unaryOp charDowncase)
    , ("char-alphabetic?", unaryOp charAlphabetic)
    , ("char-numeric?", unaryOp charNumeric)
    , ("char-whitespace?", unaryOp charWhitespace)
    , ("char-upper-case?", unaryOp charUpperCase)
    , ("char-lower-case?", unaryOp charLowerCase)
    ]

-- | Apply a unary function to a single argument
unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ args = throwError $ NumArgs 1 args

-- | Case-sensitive character comparison
charBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charBoolBinop op [Char c1, Char c2] = return $ Bool $ op c1 c2
charBoolBinop _ [Char _, notChar] = throwError $ TypeMismatch "char" notChar
charBoolBinop _ [notChar, _] = throwError $ TypeMismatch "char" notChar
charBoolBinop _ args = throwError $ NumArgs 2 args

-- | Case-insensitive character comparison
charCiBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charCiBoolBinop op [Char c1, Char c2] = return $ Bool $ op (toLower c1) (toLower c2)
charCiBoolBinop _ [Char _, notChar] = throwError $ TypeMismatch "char" notChar
charCiBoolBinop _ [notChar, _] = throwError $ TypeMismatch "char" notChar
charCiBoolBinop _ args = throwError $ NumArgs 2 args

-- | Convert character to its Unicode code point
charToInteger :: LispVal -> ThrowsError LispVal
charToInteger (Char c) = return $ Number $ SInteger $ toInteger $ fromEnum c
charToInteger notChar = throwError $ TypeMismatch "char" notChar

-- | Convert Unicode code point to character
integerToChar :: LispVal -> ThrowsError LispVal
integerToChar (Number n) = return $ Char $ toEnum $ fromIntegral $ round $ schemeNumToDouble n
integerToChar notNum = throwError $ TypeMismatch "integer" notNum

-- | Convert character to uppercase
charUpcase :: LispVal -> ThrowsError LispVal
charUpcase (Char c) = return $ Char $ toUpper c
charUpcase notChar = throwError $ TypeMismatch "char" notChar

-- | Convert character to lowercase
charDowncase :: LispVal -> ThrowsError LispVal
charDowncase (Char c) = return $ Char $ toLower c
charDowncase notChar = throwError $ TypeMismatch "char" notChar

-- | Check if character is alphabetic
charAlphabetic :: LispVal -> ThrowsError LispVal
charAlphabetic (Char c) = return $ Bool $ isAlpha c
charAlphabetic notChar = throwError $ TypeMismatch "char" notChar

-- | Check if character is numeric
charNumeric :: LispVal -> ThrowsError LispVal
charNumeric (Char c) = return $ Bool $ isDigit c
charNumeric notChar = throwError $ TypeMismatch "char" notChar

-- | Check if character is whitespace
charWhitespace :: LispVal -> ThrowsError LispVal
charWhitespace (Char c) = return $ Bool $ isSpace c
charWhitespace notChar = throwError $ TypeMismatch "char" notChar

-- | Check if character is uppercase
charUpperCase :: LispVal -> ThrowsError LispVal
charUpperCase (Char c) = return $ Bool $ isUpper c
charUpperCase notChar = throwError $ TypeMismatch "char" notChar

-- | Check if character is lowercase
charLowerCase :: LispVal -> ThrowsError LispVal
charLowerCase (Char c) = return $ Bool $ isLower c
charLowerCase notChar = throwError $ TypeMismatch "char" notChar
