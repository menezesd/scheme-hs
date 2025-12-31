{-# LANGUAGE FlexibleContexts #-}

module Eval
    ( eval
    , evalString
    , evalAndPrint
    , evalBody
    ) where

import Types
import Env
import Parser (readExpr, readExprList)
import Primitives (eqv)
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)

-- | Evaluate a string and return string result
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

-- | Evaluate and print result
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
    result <- evalString env expr
    unless (null result) $ putStrLn result

-- | Main evaluator
eval :: Env -> LispVal -> IOThrowsError LispVal

-- Self-evaluating forms
eval _ val@(Number _)  = return val
eval _ val@(String _)  = return val
eval _ val@(Char _)    = return val
eval _ val@(Bool _)    = return val
eval _ val@(Vector _)  = return val
eval _ Void            = return Void

-- Variable reference
eval env (Atom var) = getVar env var

-- Quote
eval _ (List [Atom "quote", val]) = return val

-- Quasiquote
eval env (List [Atom "quasiquote", val]) = evalQuasiquote env val 1

-- If
eval env (List [Atom "if", pred', conseq, alt]) = do
    result <- eval env pred'
    case result of
        Bool False -> eval env alt
        _          -> eval env conseq

eval env (List [Atom "if", pred', conseq]) = do
    result <- eval env pred'
    case result of
        Bool False -> return Void
        _          -> eval env conseq

-- Cond
eval env (List (Atom "cond" : clauses)) = evalCond env clauses

-- Case
eval env (List (Atom "case" : key : clauses)) = do
    keyVal <- eval env key
    evalCase env keyVal clauses

-- And
eval env (List (Atom "and" : args)) = evalAnd env args

-- Or
eval env (List (Atom "or" : args)) = evalOr env args

-- Lambda
eval env (List (Atom "lambda" : List params : body)) =
    return $ Func (map showVal params) Nothing body env

eval env (List (Atom "lambda" : DottedList params vararg : body)) =
    return $ Func (map showVal params) (Just $ showVal vararg) body env

eval env (List (Atom "lambda" : vararg@(Atom _) : body)) =
    return $ Func [] (Just $ showVal vararg) body env

-- Define variable
eval env (List [Atom "define", Atom var, form]) = do
    val <- eval env form
    defineVar env var val

-- Define function (shorthand)
eval env (List (Atom "define" : List (Atom var : params) : body)) = do
    let func = Func (map showVal params) Nothing body env
    defineVar env var func

eval env (List (Atom "define" : DottedList (Atom var : params) vararg : body)) = do
    let func = Func (map showVal params) (Just $ showVal vararg) body env
    defineVar env var func

-- Set!
eval env (List [Atom "set!", Atom var, form]) = do
    val <- eval env form
    setVar env var val

-- Let
eval env (List (Atom "let" : List bindings : body)) = do
    bindingPairs <- mapM (extractBinding env) bindings
    newEnv <- liftIO $ extendEnv env bindingPairs
    evalBody newEnv body

-- Named let
eval env (List (Atom "let" : Atom name : List bindings : body)) = do
    bindingPairs <- mapM (extractBinding env) bindings
    let paramNames = map fst bindingPairs
    -- Create environment first with placeholder for the function
    newEnv <- liftIO $ extendEnv env ((name, Void) : bindingPairs)
    -- Create function with the new environment as its closure (so it can call itself)
    let func = Func paramNames Nothing body newEnv
    -- Update the binding to the actual function
    _ <- setVar newEnv name func
    evalBody newEnv body

-- Let*
eval env (List (Atom "let*" : List bindings : body)) = do
    newEnv <- evalLetStar env bindings
    evalBody newEnv body

-- Letrec
eval env (List (Atom "letrec" : List bindings : body)) = do
    let names = map extractName bindings
    newEnv <- liftIO $ extendEnv env (map (\n -> (n, Void)) names)
    mapM_ (evalLetrecBinding newEnv) bindings
    evalBody newEnv body
  where
    extractName (List [Atom name, _]) = name
    extractName _ = ""

-- Begin
eval env (List (Atom "begin" : exprs)) = evalBody env exprs

-- Do
eval env (List (Atom "do" : List bindings : List (test : exprs) : commands)) =
    evalDo env bindings test exprs commands

-- Delay (for lazy evaluation)
eval env (List [Atom "delay", expr]) =
    return $ Func [] Nothing [expr] env

-- Define-syntax
eval env (List [Atom "define-syntax", Atom keyword, transformer]) = do
    syntax <- evalSyntaxRules transformer
    defineVar env keyword syntax

-- Let-syntax - local syntax bindings
eval env (List (Atom "let-syntax" : List bindings : body)) = do
    syntaxBindings <- mapM extractSyntaxBinding bindings
    newEnv <- liftIO $ extendEnv env syntaxBindings
    evalBody newEnv body
  where
    extractSyntaxBinding (List [Atom name, transformer]) = do
        syntax <- evalSyntaxRules transformer
        return (name, syntax)
    extractSyntaxBinding badForm = throwError $ BadSpecialForm "Invalid syntax binding" badForm

-- Letrec-syntax - recursive local syntax bindings
eval env (List (Atom "letrec-syntax" : List bindings : body)) = do
    let names = map extractName bindings
    newEnv <- liftIO $ extendEnv env (map (\n -> (n, Void)) names)
    mapM_ (evalSyntaxBinding newEnv) bindings
    evalBody newEnv body
  where
    extractName (List [Atom name, _]) = name
    extractName _ = ""
    evalSyntaxBinding env' (List [Atom name, transformer]) = do
        syntax <- evalSyntaxRules transformer
        _ <- setVar env' name syntax
        return ()
    evalSyntaxBinding _ badForm = throwError $ BadSpecialForm "Invalid syntax binding" badForm

-- Apply
eval env (List [Atom "apply", func, arg]) = do
    f <- eval env func
    a <- eval env arg
    case a of
        List args -> apply env f args
        _         -> throwError $ TypeMismatch "list" a

eval env (List (Atom "apply" : func : args)) = do
    f <- eval env func
    evaledArgs <- mapM (eval env) (init args)
    lastArg <- eval env (last args)
    case lastArg of
        List lst -> apply env f (evaledArgs ++ lst)
        _        -> throwError $ TypeMismatch "list" lastArg

-- Map
eval env (List [Atom "map", func, arg]) = do
    f <- eval env func
    lst <- eval env arg
    case lst of
        List xs -> do
            results <- mapM (\x -> apply env f [x]) xs
            return $ List results
        _ -> throwError $ TypeMismatch "list" lst

-- For-each
eval env (List [Atom "for-each", func, arg]) = do
    f <- eval env func
    lst <- eval env arg
    case lst of
        List xs -> mapM_ (\x -> apply env f [x]) xs >> return Void
        _ -> throwError $ TypeMismatch "list" lst

-- Call-with-current-continuation
eval env (List [Atom "call-with-current-continuation", proc]) = do
    f <- eval env proc
    callCC env f

eval env (List [Atom "call/cc", proc]) = do
    f <- eval env proc
    callCC env f

-- Load
eval env (List [Atom "load", String filename]) = do
    contents <- liftIO $ readFile filename
    exprs <- liftThrows $ readExprList contents
    results <- mapM (eval env) exprs
    return $ last results

-- Eval
eval env (List [Atom "eval", expr]) = do
    evaluated <- eval env expr
    eval env evaluated

eval env (List [Atom "eval", expr, envSpec]) = do
    evaluated <- eval env expr
    targetEnv <- eval env envSpec
    case targetEnv of
        List _ -> eval env evaluated  -- Fall back to current env
        _      -> eval env evaluated

-- Function application
eval env (List (func : args)) = do
    f <- eval env func
    case f of
        Syntax rules -> do
            expanded <- expandSyntax rules (List (func : args))
            eval env expanded
        Macro mParams mVararg mBody -> do
            expanded <- expandMacro mParams mVararg mBody args
            eval env expanded
        _ -> do
            evaledArgs <- mapM (eval env) args
            apply env f evaledArgs

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | Apply a function to arguments
apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ (PrimitiveFunc func) args = liftThrows $ func args
apply _ (IOFunc func) args = func args
apply _ (Func params vararg body closure) args
    | length params /= length args && isNothing vararg =
        throwError $ NumArgs (toInteger $ length params) args
    | otherwise = do
        let remainingArgs = drop (length params) args
        bindings <- return $ zip params args
        let varargBinding = case vararg of
                Nothing  -> []
                Just arg -> [(arg, List remainingArgs)]
        newEnv <- liftIO $ extendEnv closure (bindings ++ varargBinding)
        evalBody newEnv body
apply _ notFunc _ = throwError $ NotFunction "Not a function" (show notFunc)

-- | Evaluate a body (sequence of expressions)
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
evalBody _ [] = return Void
evalBody env [expr] = eval env expr
evalBody env (expr : rest) = eval env expr >> evalBody env rest

-- | Extract binding from let form
extractBinding :: Env -> LispVal -> IOThrowsError (String, LispVal)
extractBinding env (List [Atom var, expr]) = do
    val <- eval env expr
    return (var, val)
extractBinding _ badForm = throwError $ BadSpecialForm "Invalid binding" badForm

-- | Evaluate let* bindings sequentially
evalLetStar :: Env -> [LispVal] -> IOThrowsError Env
evalLetStar env [] = return env
evalLetStar env (List [Atom var, expr] : rest) = do
    val <- eval env expr
    newEnv <- liftIO $ extendEnv env [(var, val)]
    evalLetStar newEnv rest
evalLetStar _ (badForm : _) = throwError $ BadSpecialForm "Invalid binding in let*" badForm

-- | Evaluate letrec binding
evalLetrecBinding :: Env -> LispVal -> IOThrowsError ()
evalLetrecBinding env (List [Atom var, expr]) = do
    val <- eval env expr
    _ <- setVar env var val
    return ()
evalLetrecBinding _ badForm = throwError $ BadSpecialForm "Invalid binding in letrec" badForm

-- | Evaluate cond clauses
evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond _ [] = return Void
evalCond env [List (Atom "else" : exprs)] = evalBody env exprs
evalCond env (List [test, Atom "=>", expr] : rest) = do
    result <- eval env test
    case result of
        Bool False -> evalCond env rest
        _          -> do
            func <- eval env expr
            apply env func [result]
evalCond env (List (test : exprs) : rest) = do
    result <- eval env test
    case result of
        Bool False -> evalCond env rest
        _          -> if null exprs
                        then return result
                        else evalBody env exprs
evalCond _ (badClause : _) = throwError $ BadSpecialForm "Invalid cond clause" badClause

-- | Evaluate case clauses
evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase _ _ [] = return Void
evalCase env _ [List (Atom "else" : exprs)] = evalBody env exprs
evalCase env keyVal (List (List datums : exprs) : rest) = do
    matches <- mapM (\d -> liftThrows $ eqv [keyVal, d]) datums
    if any isBoolTrue matches
        then evalBody env exprs
        else evalCase env keyVal rest
  where
    isBoolTrue (Bool True) = True
    isBoolTrue _           = False
evalCase _ _ (badClause : _) = throwError $ BadSpecialForm "Invalid case clause" badClause

-- | Evaluate and expression
evalAnd :: Env -> [LispVal] -> IOThrowsError LispVal
evalAnd _ [] = return $ Bool True
evalAnd env [x] = eval env x
evalAnd env (x : xs) = do
    result <- eval env x
    case result of
        Bool False -> return $ Bool False
        _          -> evalAnd env xs

-- | Evaluate or expression
evalOr :: Env -> [LispVal] -> IOThrowsError LispVal
evalOr _ [] = return $ Bool False
evalOr env [x] = eval env x
evalOr env (x : xs) = do
    result <- eval env x
    case result of
        Bool False -> evalOr env xs
        _          -> return result

-- | Evaluate do loop
evalDo :: Env -> [LispVal] -> LispVal -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalDo env bindings test exprs commands = do
    -- Initialize variables
    initBindings <- mapM extractDoBinding bindings
    loopEnv <- liftIO $ extendEnv env initBindings
    doLoop loopEnv bindings test exprs commands
  where
    extractDoBinding (List [Atom var, init', _]) = do
        val <- eval env init'
        return (var, val)
    extractDoBinding (List [Atom var, init']) = do
        val <- eval env init'
        return (var, val)
    extractDoBinding badForm = throwError $ BadSpecialForm "Invalid do binding" badForm

doLoop :: Env -> [LispVal] -> LispVal -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
doLoop env bindings test exprs commands = do
    testResult <- eval env test
    case testResult of
        Bool False -> do
            -- Execute commands
            mapM_ (eval env) commands
            -- Update step expressions
            newVals <- mapM (evalStep env) bindings
            let varNames = map extractVarName bindings
            mapM_ (\(var, val) -> setVar env var val) (zip varNames newVals)
            doLoop env bindings test exprs commands
        _ -> evalBody env exprs
  where
    extractVarName (List (Atom var : _)) = var
    extractVarName _ = ""
    evalStep env' (List [_, _, step]) = eval env' step
    evalStep env' (List [Atom var, _]) = getVar env' var
    evalStep _ _ = return Void

-- | Evaluate quasiquote
evalQuasiquote :: Env -> LispVal -> Int -> IOThrowsError LispVal
evalQuasiquote _ val 0 = return val
evalQuasiquote env (List [Atom "unquote", val]) 1 = eval env val
evalQuasiquote env (List [Atom "unquote", val]) n =
    evalQuasiquote env val (n - 1) >>= \v -> return $ List [Atom "unquote", v]
evalQuasiquote env (List [Atom "quasiquote", val]) n = do
    v <- evalQuasiquote env val (n + 1)
    return $ List [Atom "quasiquote", v]
evalQuasiquote env (List vals) n = do
    expanded <- mapM (expandQuasiquoteList env n) vals
    return $ List $ concat expanded
evalQuasiquote env (DottedList h t) n = do
    expandedH <- mapM (expandQuasiquoteList env n) h
    expandedT <- evalQuasiquote env t n
    return $ DottedList (concat expandedH) expandedT
evalQuasiquote _ val _ = return val

expandQuasiquoteList :: Env -> Int -> LispVal -> IOThrowsError [LispVal]
expandQuasiquoteList env 1 (List [Atom "unquote-splicing", val]) = do
    result <- eval env val
    case result of
        List xs -> return xs
        _       -> throwError $ TypeMismatch "list" result
expandQuasiquoteList env n val = do
    expanded <- evalQuasiquote env val n
    return [expanded]

-- | Evaluate syntax-rules transformer
evalSyntaxRules :: LispVal -> IOThrowsError LispVal
evalSyntaxRules (List (Atom "syntax-rules" : List literals : rules)) = do
    let literalNames = map extractLiteralName literals
    rulesList <- mapM extractRule rules
    return $ Syntax $ SyntaxRules literalNames rulesList
  where
    extractLiteralName (Atom name) = name
    extractLiteralName _ = ""
    extractRule (List [pattern, template]) = return (pattern, template)
    extractRule badForm = throwError $ BadSpecialForm "Invalid syntax rule" badForm
evalSyntaxRules badForm = throwError $ BadSpecialForm "Invalid syntax-rules" badForm

-- | Expand syntax-rules macro
expandSyntax :: SyntaxRules -> LispVal -> IOThrowsError LispVal
expandSyntax (SyntaxRules lits rules) form = tryRules rules
  where
    tryRules [] = throwError $ BadSpecialForm "No matching syntax rule" form
    tryRules ((pattern, template) : rest) =
        case matchPattern lits pattern form of
            Nothing       -> tryRules rest
            Just bindings -> return $ substituteTemplate bindings template

-- | Match pattern against form, returning bindings
matchPattern :: [String] -> LispVal -> LispVal -> Maybe [(String, LispVal)]
matchPattern _ (Atom "_") _ = Just []
matchPattern lits (Atom var) form
    | var `elem` lits = if form == Atom var then Just [] else Nothing
    | otherwise       = Just [(var, form)]
matchPattern lits (List [p, Atom "..."]) (List forms) =
    matchEllipsis lits p forms
matchPattern lits (List patterns) (List forms)
    | length patterns == length forms =
        fmap concat $ sequence $ zipWith (matchPattern lits) patterns forms
    | otherwise = Nothing
matchPattern lits (DottedList pats pTail) (DottedList forms fTail)
    | length pats == length forms = do
        headBindings <- fmap concat $ sequence $ zipWith (matchPattern lits) pats forms
        tailBindings <- matchPattern lits pTail fTail
        return $ headBindings ++ tailBindings
matchPattern lits (DottedList pats pTail) (List forms)
    | length forms >= length pats = do
        let (headForms, tailForms) = splitAt (length pats) forms
        headBindings <- fmap concat $ sequence $ zipWith (matchPattern lits) pats headForms
        tailBindings <- matchPattern lits pTail (List tailForms)
        return $ headBindings ++ tailBindings
matchPattern _ pat form
    | pat == form = Just []
    | otherwise   = Nothing

-- | Match ellipsis pattern
matchEllipsis :: [String] -> LispVal -> [LispVal] -> Maybe [(String, LispVal)]
matchEllipsis lits pattern forms = do
    allBindings <- mapM (matchPattern lits pattern) forms
    let vars = getPatternVars pattern
    let grouped = [(v, List $ map (lookupBinding v) allBindings) | v <- vars]
    return grouped
  where
    lookupBinding var bindings = case lookup var bindings of
        Just val -> val
        Nothing  -> Void

-- | Get variables from pattern
getPatternVars :: LispVal -> [String]
getPatternVars (Atom "_") = []
getPatternVars (Atom var) = [var]
getPatternVars (List pats) = concatMap getPatternVars pats
getPatternVars (DottedList pats tail') = concatMap getPatternVars pats ++ getPatternVars tail'
getPatternVars _ = []

-- | Substitute bindings into template
substituteTemplate :: [(String, LispVal)] -> LispVal -> LispVal
substituteTemplate bindings (Atom var) =
    case lookup var bindings of
        Just val -> val
        Nothing  -> Atom var
substituteTemplate bindings (List [tmpl, Atom "..."]) =
    case expandEllipsisTemplate bindings tmpl of
        Just vals -> List vals
        Nothing   -> List [substituteTemplate bindings tmpl]
substituteTemplate bindings (List tmpls) =
    List $ concatMap (expandListElement bindings) tmpls
substituteTemplate bindings (DottedList tmpls tail') =
    DottedList (map (substituteTemplate bindings) tmpls) (substituteTemplate bindings tail')
substituteTemplate _ val = val

-- | Expand list element (handle ellipsis)
expandListElement :: [(String, LispVal)] -> LispVal -> [LispVal]
expandListElement bindings (List [tmpl, Atom "..."]) =
    case expandEllipsisTemplate bindings tmpl of
        Just vals -> vals
        Nothing   -> []
expandListElement bindings tmpl = [substituteTemplate bindings tmpl]

-- | Expand ellipsis template
expandEllipsisTemplate :: [(String, LispVal)] -> LispVal -> Maybe [LispVal]
expandEllipsisTemplate bindings tmpl = do
    let vars = getPatternVars tmpl
    let listBindings = [(v, xs) | v <- vars, Just (List xs) <- [lookup v bindings]]
    if null listBindings
        then Nothing
        else do
            let n = minimum $ map (length . snd) listBindings
            return [substituteTemplate (makeBindings i listBindings bindings) tmpl | i <- [0..n-1]]
  where
    makeBindings i listBinds otherBinds =
        [(v, xs !! i) | (v, xs) <- listBinds] ++
        [(v, val) | (v, val) <- otherBinds, v `notElem` map fst listBinds]

-- | Expand a traditional macro
expandMacro :: [String] -> Maybe String -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
expandMacro params vararg body args
    | length params /= length args && isNothing vararg =
        throwError $ NumArgs (toInteger $ length params) args
    | otherwise = do
        let remainingArgs = drop (length params) args
        let bindings = zip params args
        let varargBinding = case vararg of
                Nothing  -> []
                Just arg -> [(arg, List remainingArgs)]
        let allBindings = bindings ++ varargBinding
        return $ substituteTemplate allBindings (List (Atom "begin" : body))

-- | Implement call/cc using exception-based continuations
callCC :: Env -> LispVal -> IOThrowsError LispVal
callCC env proc = do
    -- Create a continuation that throws a ContinuationJump when called
    let continuation = IOFunc $ \args -> case args of
            [val] -> throwError $ ContinuationJump val
            _     -> throwError $ NumArgs 1 args
    -- Apply the procedure to the continuation and catch any jumps
    catchError (apply env proc [continuation]) $ \err ->
        case err of
            ContinuationJump val -> return val
            _                    -> throwError err
