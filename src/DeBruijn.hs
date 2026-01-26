{-# LANGUAGE BangPatterns #-}

-- | De Bruijn Index Compilation
--
-- This module implements lexical address computation for the Scheme interpreter.
-- De Bruijn indices replace variable names with numeric addresses (depth, offset),
-- enabling O(1) variable lookup instead of O(log n) hash map lookup.
--
-- == How It Works
--
-- A lexical address (depth, offset) encodes:
--
-- * @depth@: Number of enclosing scopes to traverse (0 = current frame)
-- * @offset@: Index within that frame
--
-- Example:
--
-- @
-- (lambda (x y)        ; depth 0: x at offset 0, y at offset 1
--   (lambda (z)        ; depth 0: z at offset 0
--     (+ x y z)))      ; x is (1,0), y is (1,1), z is (0,0)
-- @
--
-- == Usage
--
-- The primary entry point is 'annotateExpr', which transforms an AST
-- replacing variable references with 'LexicalRef' values.
module DeBruijn
    ( -- * Compilation environment
      LexEnv
    , emptyLexEnv
    , extendLexEnv
    , lookupLexEnv
      -- * AST annotation
    , annotateExpr
    , annotateBody
    ) where

import Types
import Control.Monad.Except
import qualified Data.Map.Strict as Map

-- | Compile-time environment for tracking variable bindings
--
-- Maps variable names to their lexical addresses.
-- The environment is a list of frames, where each frame is a list of bindings.
type LexEnv = [[String]]

-- | Empty lexical environment
emptyLexEnv :: LexEnv
emptyLexEnv = []

-- | Extend the environment with a new frame of bindings
extendLexEnv :: [String] -> LexEnv -> LexEnv
extendLexEnv bindings env = bindings : env

-- | Look up a variable in the lexical environment
--
-- Returns 'Just' the address if found, 'Nothing' if unbound (global/primitive)
lookupLexEnv :: String -> LexEnv -> Maybe LexicalAddress
lookupLexEnv name env = go 0 env
  where
    go _ [] = Nothing
    go depth (frame:rest) =
        case findIndex name frame of
            Just offset -> Just $ LexicalAddress depth offset
            Nothing -> go (depth + 1) rest

    findIndex _ [] = Nothing
    findIndex n (x:xs)
        | n == x = Just 0
        | otherwise = (1+) <$> findIndex n xs

-- | Annotate an expression with lexical addresses
--
-- Transforms variable references (Atom) to LexicalRef where the variable
-- is bound in the lexical environment. Unbound variables remain as Atom
-- (they will be resolved at runtime from the global environment).
annotateExpr :: LexEnv -> LispVal -> ThrowsError LispVal
annotateExpr env expr = case expr of
    -- Variable reference - check if lexically bound
    Atom name -> case lookupLexEnv name env of
        Just addr -> return $ LexicalRef name addr
        Nothing -> return $ Atom name  -- Global/primitive

    -- Self-evaluating forms - no change needed
    val@(Number _) -> return val
    val@(String _) -> return val
    val@(Char _) -> return val
    val@(Bool _) -> return val
    val@(Vector _) -> return val
    val@Void -> return val
    val@EOF -> return val

    -- Quote - don't annotate quoted data
    List [Atom "quote", val] -> return $ List [Atom "quote", val]

    -- Quasiquote - need special handling
    List [Atom "quasiquote", val] -> do
        val' <- annotateQuasiquote env val 1
        return $ List [Atom "quasiquote", val']

    -- Lambda - extend environment with parameters
    List (Atom "lambda" : List params : body) -> do
        paramNames <- mapM extractParam params
        let env' = extendLexEnv paramNames env
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "lambda" : List params : body')

    List (Atom "lambda" : DottedList params rest : body) -> do
        paramNames <- mapM extractParam params
        restName <- extractParam rest
        let env' = extendLexEnv (paramNames ++ [restName]) env
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "lambda" : DottedList params rest : body')

    -- Let - evaluate bindings in current env, body in extended env
    List (Atom "let" : List bindings : body) -> do
        (names, exprs) <- unzipBindings bindings
        exprs' <- mapM (annotateExpr env) exprs
        let env' = extendLexEnv names env
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "let" : List (zipBindings names exprs') : body')

    -- Named let - recursive, name visible in body
    List (Atom "let" : Atom name : List bindings : body) -> do
        (names, exprs) <- unzipBindings bindings
        exprs' <- mapM (annotateExpr env) exprs
        let env' = extendLexEnv (name : names) env
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "let" : Atom name : List (zipBindings names exprs') : body')

    -- Let* - sequential bindings
    List (Atom "let*" : List bindings : body) -> do
        bindings' <- annotateLetStar env bindings
        let names = extractBindingNames bindings
            env' = extendLexEnv names env
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "let*" : List bindings' : body')

    -- Letrec - all names visible in all bindings
    List (Atom "letrec" : List bindings : body) -> do
        let names = extractBindingNames bindings
            env' = extendLexEnv names env
        bindings' <- annotateBindings env' bindings
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "letrec" : List bindings' : body')

    -- Do - complex binding with step expressions
    List (Atom "do" : List bindings : test : body) -> do
        let names = extractDoNames bindings
            env' = extendLexEnv names env
        bindings' <- annotateDoBindings env env' bindings
        test' <- annotateExpr env' test
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "do" : List bindings' : test' : body')

    -- Define - special handling for internal defines
    List [Atom "define", Atom var, val] -> do
        val' <- annotateExpr env val
        return $ List [Atom "define", Atom var, val']

    List (Atom "define" : List (Atom name : params) : body) -> do
        paramNames <- mapM extractParam params
        let env' = extendLexEnv paramNames env
        body' <- mapM (annotateExpr env') body
        return $ List (Atom "define" : List (Atom name : params) : body')

    -- Set! - annotate both variable and value
    List [Atom "set!", Atom var, val] -> do
        val' <- annotateExpr env val
        case lookupLexEnv var env of
            Just addr -> return $ List [Atom "set!", LexicalRef var addr, val']
            Nothing -> return $ List [Atom "set!", Atom var, val']

    -- If
    List [Atom "if", pred', conseq] -> do
        pred'' <- annotateExpr env pred'
        conseq' <- annotateExpr env conseq
        return $ List [Atom "if", pred'', conseq']

    List [Atom "if", pred', conseq, alt] -> do
        pred'' <- annotateExpr env pred'
        conseq' <- annotateExpr env conseq
        alt' <- annotateExpr env alt
        return $ List [Atom "if", pred'', conseq', alt']

    -- Begin
    List (Atom "begin" : exprs) -> do
        exprs' <- mapM (annotateExpr env) exprs
        return $ List (Atom "begin" : exprs')

    -- And/Or
    List (Atom "and" : exprs) -> do
        exprs' <- mapM (annotateExpr env) exprs
        return $ List (Atom "and" : exprs')

    List (Atom "or" : exprs) -> do
        exprs' <- mapM (annotateExpr env) exprs
        return $ List (Atom "or" : exprs')

    -- Cond
    List (Atom "cond" : clauses) -> do
        clauses' <- mapM (annotateCondClause env) clauses
        return $ List (Atom "cond" : clauses')

    -- Case
    List (Atom "case" : key : clauses) -> do
        key' <- annotateExpr env key
        clauses' <- mapM (annotateCaseClause env) clauses
        return $ List (Atom "case" : key' : clauses')

    -- General application
    List (func : args) -> do
        func' <- annotateExpr env func
        args' <- mapM (annotateExpr env) args
        return $ List (func' : args')

    -- Lists without special handling
    List [] -> return $ List []

    -- Dotted lists
    DottedList head' tail' -> do
        head'' <- mapM (annotateExpr env) head'
        tail'' <- annotateExpr env tail'
        return $ DottedList head'' tail''

    -- Everything else passes through
    other -> return other

-- | Annotate a body (list of expressions)
annotateBody :: LexEnv -> [LispVal] -> ThrowsError [LispVal]
annotateBody env = mapM (annotateExpr env)

-- Helper functions

extractParam :: LispVal -> ThrowsError String
extractParam (Atom name) = return name
extractParam badVal = throwError $ TypeMismatch "symbol" badVal

unzipBindings :: [LispVal] -> ThrowsError ([String], [LispVal])
unzipBindings bindings = do
    pairs <- mapM extractBinding bindings
    return (map fst pairs, map snd pairs)

extractBinding :: LispVal -> ThrowsError (String, LispVal)
extractBinding (List [Atom name, expr]) = return (name, expr)
extractBinding badForm = throwError $ BadSpecialForm "Invalid binding" badForm

extractBindingNames :: [LispVal] -> [String]
extractBindingNames = map getName
  where
    getName (List [Atom name, _]) = name
    getName _ = ""

extractDoNames :: [LispVal] -> [String]
extractDoNames = map getName
  where
    getName (List (Atom name : _)) = name
    getName _ = ""

zipBindings :: [String] -> [LispVal] -> [LispVal]
zipBindings names exprs = [List [Atom n, e] | (n, e) <- zip names exprs]

annotateBindings :: LexEnv -> [LispVal] -> ThrowsError [LispVal]
annotateBindings env = mapM annotateBinding
  where
    annotateBinding (List [Atom name, expr]) = do
        expr' <- annotateExpr env expr
        return $ List [Atom name, expr']
    annotateBinding badForm = throwError $ BadSpecialForm "Invalid binding" badForm

annotateLetStar :: LexEnv -> [LispVal] -> ThrowsError [LispVal]
annotateLetStar _ [] = return []
annotateLetStar env (List [Atom name, expr] : rest) = do
    expr' <- annotateExpr env expr
    let env' = extendLexEnv [name] env
    rest' <- annotateLetStar env' rest
    return $ List [Atom name, expr'] : rest'
annotateLetStar _ (badForm : _) = throwError $ BadSpecialForm "Invalid binding" badForm

annotateDoBindings :: LexEnv -> LexEnv -> [LispVal] -> ThrowsError [LispVal]
annotateDoBindings initEnv stepEnv = mapM annotateDoBinding
  where
    annotateDoBinding (List [Atom name, init']) = do
        init'' <- annotateExpr initEnv init'
        return $ List [Atom name, init'']
    annotateDoBinding (List [Atom name, init', step]) = do
        init'' <- annotateExpr initEnv init'
        step' <- annotateExpr stepEnv step
        return $ List [Atom name, init'', step']
    annotateDoBinding badForm = throwError $ BadSpecialForm "Invalid do binding" badForm

annotateCondClause :: LexEnv -> LispVal -> ThrowsError LispVal
annotateCondClause env (List (Atom "else" : exprs)) = do
    exprs' <- mapM (annotateExpr env) exprs
    return $ List (Atom "else" : exprs')
annotateCondClause env (List (test : exprs)) = do
    test' <- annotateExpr env test
    exprs' <- mapM (annotateExpr env) exprs
    return $ List (test' : exprs')
annotateCondClause _ badForm = throwError $ BadSpecialForm "Invalid cond clause" badForm

annotateCaseClause :: LexEnv -> LispVal -> ThrowsError LispVal
annotateCaseClause env (List (Atom "else" : exprs)) = do
    exprs' <- mapM (annotateExpr env) exprs
    return $ List (Atom "else" : exprs')
annotateCaseClause env (List (List datums : exprs)) = do
    exprs' <- mapM (annotateExpr env) exprs
    return $ List (List datums : exprs')  -- datums are not expressions
annotateCaseClause _ badForm = throwError $ BadSpecialForm "Invalid case clause" badForm

annotateQuasiquote :: LexEnv -> LispVal -> Int -> ThrowsError LispVal
annotateQuasiquote env val level
    | level == 0 = annotateExpr env val
    | otherwise = case val of
        List [Atom "unquote", inner]
            | level == 1 -> do
                inner' <- annotateExpr env inner
                return $ List [Atom "unquote", inner']
            | otherwise -> do
                inner' <- annotateQuasiquote env inner (level - 1)
                return $ List [Atom "unquote", inner']
        List [Atom "quasiquote", inner] -> do
            inner' <- annotateQuasiquote env inner (level + 1)
            return $ List [Atom "quasiquote", inner']
        List items -> do
            items' <- mapM (annotateQuasiquote env `flip` level) items
            return $ List items'
        DottedList h t -> do
            h' <- mapM (annotateQuasiquote env `flip` level) h
            t' <- annotateQuasiquote env t level
            return $ DottedList h' t'
        _ -> return val
