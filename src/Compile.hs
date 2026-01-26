{-# LANGUAGE RecordWildCards #-}

-- | Bytecode Compiler for Scheme
--
-- This module compiles Scheme expressions to bytecode for the VM.
-- The compilation process handles:
--
-- * Variable resolution using De Bruijn indices
-- * Tail call detection for proper TCO
-- * Closure creation for nested functions
-- * Constant folding for literals
--
-- == Usage
--
-- @
-- code <- compile env expr
-- result <- runVM code
-- @
module Compile
    ( -- * Compilation
      compile
    , compileTopLevel
    , compileLambda
      -- * Compiler state
    , CompileState
    ) where

import Types
import Bytecode
import DeBruijn

import Control.Monad.State
import Control.Monad.Except
import Data.IORef
import qualified Data.Vector as V
import Data.List (elemIndex)
import Data.Complex (Complex(..))

-- | Compiler state
data CompileState = CompileState
    { csCode      :: [Opcode]         -- Emitted code (reverse order)
    , csConstants :: [ConstVal]       -- Constants (reverse order)
    , csChildren  :: [CodeObject]     -- Child closures (reverse order)
    , csLexEnv    :: LexEnv           -- Lexical environment
    , csLocals    :: Int              -- Number of locals in current frame
    , csTailPos   :: Bool             -- Are we in tail position?
    }

-- | Initial compiler state
initialState :: LexEnv -> CompileState
initialState env = CompileState
    { csCode = []
    , csConstants = []
    , csChildren = []
    , csLexEnv = env
    , csLocals = 0
    , csTailPos = False
    }

-- | Compiler monad
type Compiler a = StateT CompileState (Either LispError) a

-- | Compile a top-level expression
compileTopLevel :: LispVal -> Either LispError CodeObject
compileTopLevel expr = do
    let annotated = annotateExpr emptyLexEnv expr
    expr' <- annotated
    compile' expr'

-- | Compile an expression (entry point)
compile :: LexEnv -> LispVal -> Either LispError CodeObject
compile env expr = do
    expr' <- annotateExpr env expr
    execStateT (compileExpr expr' >> emit OP_HALT) (initialState env)
        >>= buildCodeObject

-- | Internal compilation
compile' :: LispVal -> Either LispError CodeObject
compile' expr = do
    execStateT (compileExpr expr >> emit OP_HALT) (initialState emptyLexEnv)
        >>= buildCodeObject

-- | Build a CodeObject from compiler state
buildCodeObject :: CompileState -> Either LispError CodeObject
buildCodeObject CompileState{..} = return CodeObject
    { coCode = V.fromList (reverse csCode)
    , coConstants = V.fromList (reverse csConstants)
    , coChildren = V.fromList (reverse csChildren)
    , coArity = 0
    , coHasRest = False
    , coName = Nothing
    , coLocals = csLocals
    }

-- | Emit an opcode
emit :: Opcode -> Compiler ()
emit op = modify $ \s -> s { csCode = op : csCode s }

-- | Add a constant and return its index
addConstant :: ConstVal -> Compiler Int
addConstant c = do
    consts <- gets csConstants
    case elemIndex c (reverse consts) of
        Just idx -> return idx
        Nothing -> do
            let idx = length consts
            modify $ \s -> s { csConstants = c : csConstants s }
            return idx

-- | Add a child closure and return its index
addChild :: CodeObject -> Compiler Int
addChild child = do
    children <- gets csChildren
    let idx = length children
    modify $ \s -> s { csChildren = child : csChildren s }
    return idx

-- | Get current code position
currentPos :: Compiler Int
currentPos = length <$> gets csCode

-- | Patch a jump instruction
patchJump :: Int -> Compiler ()
patchJump pos = do
    target <- currentPos
    code <- gets csCode
    let len = length code
        idx = len - 1 - pos
        code' = take idx code ++ [patchOp (code !! idx) target] ++ drop (idx + 1) code
    modify $ \s -> s { csCode = code' }
  where
    patchOp (OP_JUMP _) t = OP_JUMP t
    patchOp (OP_JUMPIF _) t = OP_JUMPIF t
    patchOp (OP_JUMPIFNOT _) t = OP_JUMPIFNOT t
    patchOp op _ = op

-- | Set tail position flag
withTailPosition :: Bool -> Compiler a -> Compiler a
withTailPosition tp action = do
    old <- gets csTailPos
    modify $ \s -> s { csTailPos = tp }
    result <- action
    modify $ \s -> s { csTailPos = old }
    return result

-- | Compile an expression
compileExpr :: LispVal -> Compiler ()
compileExpr expr = case expr of
    -- Constants
    Number (SInteger n) -> do
        idx <- addConstant (CNumber n)
        emit $ OP_CONST idx

    Number (SRational r) -> do
        idx <- addConstant (CRational (toRational r))
        emit $ OP_CONST idx

    Number (SReal d) -> do
        idx <- addConstant (CReal d)
        emit $ OP_CONST idx

    Number (SComplex c) -> do
        let r = realPart c
            i = imagPart c
        idx <- addConstant (CComplex r i)
        emit $ OP_CONST idx
      where
        realPart (r :+ _) = r
        imagPart (_ :+ i) = i

    String s -> do
        idx <- addConstant (CString s)
        emit $ OP_CONST idx

    Char c -> do
        idx <- addConstant (CChar c)
        emit $ OP_CONST idx

    Bool b -> do
        idx <- addConstant (CBool b)
        emit $ OP_CONST idx

    -- Quoted values
    List [Atom "quote", val] -> compileQuoted val

    -- Variables
    LexicalRef _ (LexicalAddress depth offset) ->
        emit $ OP_LOOKUP depth offset

    Atom name -> do
        idx <- addConstant (CSymbol name)
        emit $ OP_LOOKUP_GLOBAL idx

    -- Void
    Void -> emit OP_VOID

    -- Lambda
    List (Atom "lambda" : List params : body) -> do
        env <- gets csLexEnv
        let paramNames = map getAtomName params
            env' = extendLexEnv paramNames env
        childCode <- lift $ compileLambda env' paramNames False body
        childIdx <- addChild childCode
        emit $ OP_CLOSURE childIdx

    List (Atom "lambda" : DottedList params rest : body) -> do
        env <- gets csLexEnv
        let paramNames = map getAtomName params ++ [getAtomName rest]
            env' = extendLexEnv paramNames env
        childCode <- lift $ compileLambda env' paramNames True body
        childIdx <- addChild childCode
        emit $ OP_CLOSURE childIdx

    -- If
    List [Atom "if", pred', conseq] -> do
        withTailPosition False $ compileExpr pred'
        emit $ OP_JUMPIFNOT 0
        jumpPos <- currentPos
        compileExpr conseq
        patchJump (jumpPos - 1)

    List [Atom "if", pred', conseq, alt] -> do
        withTailPosition False $ compileExpr pred'
        emit $ OP_JUMPIFNOT 0
        jumpToAlt <- currentPos
        compileExpr conseq
        emit $ OP_JUMP 0
        jumpToEnd <- currentPos
        patchJump (jumpToAlt - 1)
        compileExpr alt
        patchJump (jumpToEnd - 1)

    -- Begin
    List (Atom "begin" : exprs) -> compileSequence exprs

    -- Let
    List (Atom "let" : List bindings : body) -> do
        -- Compile binding values
        mapM_ compileBinding bindings
        -- Extend environment and compile body
        env <- gets csLexEnv
        let names = map getBindingName bindings
            env' = extendLexEnv names env
        modify $ \s -> s { csLexEnv = env' }
        compileSequence body
        modify $ \s -> s { csLexEnv = env }
      where
        compileBinding (List [_, val]) = withTailPosition False $ compileExpr val
        compileBinding _ = return ()

    -- Set!
    List [Atom "set!", LexicalRef _ (LexicalAddress depth offset), val] -> do
        compileExpr val
        emit $ OP_SET depth offset

    List [Atom "set!", Atom name, val] -> do
        compileExpr val
        idx <- addConstant (CSymbol name)
        emit $ OP_SET_GLOBAL idx

    -- Define
    List [Atom "define", Atom name, val] -> do
        compileExpr val
        idx <- addConstant (CSymbol name)
        emit $ OP_CONST idx
        emit OP_DEFINE

    -- And
    List (Atom "and" : []) -> do
        idx <- addConstant (CBool True)
        emit $ OP_CONST idx

    List (Atom "and" : [e]) -> compileExpr e

    List (Atom "and" : e : es) -> do
        withTailPosition False $ compileExpr e
        emit $ OP_JUMPIFNOT 0
        jumpPos <- currentPos
        compileExpr (List (Atom "and" : es))
        emit $ OP_JUMP 0
        endPos <- currentPos
        patchJump (jumpPos - 1)
        idx <- addConstant (CBool False)
        emit $ OP_CONST idx
        patchJump (endPos - 1)

    -- Or
    List (Atom "or" : []) -> do
        idx <- addConstant (CBool False)
        emit $ OP_CONST idx

    List (Atom "or" : [e]) -> compileExpr e

    List (Atom "or" : e : es) -> do
        withTailPosition False $ compileExpr e
        emit OP_DUP
        emit $ OP_JUMPIF 0
        jumpPos <- currentPos
        emit OP_POP
        compileExpr (List (Atom "or" : es))
        patchJump (jumpPos - 1)

    -- Function application
    List (func : args) -> do
        -- Compile arguments first
        mapM_ (\arg -> withTailPosition False $ compileExpr arg) args
        -- Compile function
        withTailPosition False $ compileExpr func
        -- Emit call
        tailPos <- gets csTailPos
        if tailPos
            then emit $ OP_TAILCALL (length args)
            else emit $ OP_CALL (length args)

    -- Empty list
    List [] -> do
        idx <- addConstant CNull
        emit $ OP_CONST idx

    -- Default: treat as self-evaluating
    _ -> do
        idx <- addConstant CVoid
        emit $ OP_CONST idx

-- | Compile a quoted value
compileQuoted :: LispVal -> Compiler ()
compileQuoted val = case val of
    Number (SInteger n) -> do
        idx <- addConstant (CNumber n)
        emit $ OP_CONST idx
    String s -> do
        idx <- addConstant (CString s)
        emit $ OP_CONST idx
    Char c -> do
        idx <- addConstant (CChar c)
        emit $ OP_CONST idx
    Bool b -> do
        idx <- addConstant (CBool b)
        emit $ OP_CONST idx
    Atom s -> do
        idx <- addConstant (CSymbol s)
        emit $ OP_CONST idx
    List [] -> do
        idx <- addConstant CNull
        emit $ OP_CONST idx
    List xs -> do
        mapM_ compileQuoted xs
        emit $ OP_LIST (length xs)
    _ -> do
        idx <- addConstant CVoid
        emit $ OP_CONST idx

-- | Compile a sequence of expressions
compileSequence :: [LispVal] -> Compiler ()
compileSequence [] = emit OP_VOID
compileSequence [e] = compileExpr e
compileSequence (e:es) = do
    withTailPosition False $ compileExpr e
    emit OP_POP
    compileSequence es

-- | Compile a lambda body
compileLambda :: LexEnv -> [String] -> Bool -> [LispVal] -> Either LispError CodeObject
compileLambda env params hasRest body = do
    annotatedBody <- mapM (annotateExpr env) body
    let state = (initialState env) { csTailPos = True }
    state' <- execStateT (compileSequence annotatedBody >> emit OP_RETURN) state
    let code = buildCodeObject state'
    case code of
        Left err -> Left err
        Right co -> Right co
            { coArity = if hasRest then length params - 1 else length params
            , coHasRest = hasRest
            }

-- Helper functions

getAtomName :: LispVal -> String
getAtomName (Atom n) = n
getAtomName _ = ""

getBindingName :: LispVal -> String
getBindingName (List [Atom n, _]) = n
getBindingName _ = ""
