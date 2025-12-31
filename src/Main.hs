module Main where

import Types
import Parser (readExprList)
import Env
import Eval
import Primitives (primitiveBindings)
import Control.Monad (unless)
import Control.Monad.Except
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        []         -> runRepl
        [filename] -> runFile filename
        _          -> putStrLn "Usage: scheme-hs [filename]"

-- | Standard library definitions
stdlib :: String
stdlib = unlines
    [ ";; Deep car/cdr accessors (R5RS)"
    , "(define (caar x) (car (car x)))"
    , "(define (cadr x) (car (cdr x)))"
    , "(define (cdar x) (cdr (car x)))"
    , "(define (cddr x) (cdr (cdr x)))"
    , "(define (caaar x) (car (car (car x))))"
    , "(define (caadr x) (car (car (cdr x))))"
    , "(define (cadar x) (car (cdr (car x))))"
    , "(define (caddr x) (car (cdr (cdr x))))"
    , "(define (cdaar x) (cdr (car (car x))))"
    , "(define (cdadr x) (cdr (car (cdr x))))"
    , "(define (cddar x) (cdr (cdr (car x))))"
    , "(define (cdddr x) (cdr (cdr (cdr x))))"
    , ";; Four-level deep accessors"
    , "(define (caaaar x) (car (car (car (car x)))))"
    , "(define (caaadr x) (car (car (car (cdr x)))))"
    , "(define (caadar x) (car (car (cdr (car x)))))"
    , "(define (caaddr x) (car (car (cdr (cdr x)))))"
    , "(define (cadaar x) (car (cdr (car (car x)))))"
    , "(define (cadadr x) (car (cdr (car (cdr x)))))"
    , "(define (caddar x) (car (cdr (cdr (car x)))))"
    , "(define (cadddr x) (car (cdr (cdr (cdr x)))))"
    , "(define (cdaaar x) (cdr (car (car (car x)))))"
    , "(define (cdaadr x) (cdr (car (car (cdr x)))))"
    , "(define (cdadar x) (cdr (car (cdr (car x)))))"
    , "(define (cdaddr x) (cdr (car (cdr (cdr x)))))"
    , "(define (cddaar x) (cdr (cdr (car (car x)))))"
    , "(define (cddadr x) (cdr (cdr (car (cdr x)))))"
    , "(define (cdddar x) (cdr (cdr (cdr (car x)))))"
    , "(define (cddddr x) (cdr (cdr (cdr (cdr x)))))"
    , ""
    , "(define (list? x)"
    , "  (or (null? x)"
    , "      (and (pair? x) (list? (cdr x)))))"
    , ""
    , "(define (force promise) (promise))"
    , ""
    , "(define (newline) (display \"\\n\"))"
    , ""
    , "(define (call-with-values producer consumer)"
    , "  (apply consumer (producer)))"
    , ""
    , "(define (values . args) args)"
    , ""
    , "(define (fold-left f init lst)"
    , "  (if (null? lst)"
    , "      init"
    , "      (fold-left f (f init (car lst)) (cdr lst))))"
    , ""
    , "(define (fold-right f init lst)"
    , "  (if (null? lst)"
    , "      init"
    , "      (f (car lst) (fold-right f init (cdr lst)))))"
    , ""
    , "(define foldl fold-left)"
    , "(define foldr fold-right)"
    , ""
    , "(define (filter pred lst)"
    , "  (cond ((null? lst) '())"
    , "        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))"
    , "        (else (filter pred (cdr lst)))))"
    , ""
    , "(define (any pred lst)"
    , "  (cond ((null? lst) #f)"
    , "        ((pred (car lst)) #t)"
    , "        (else (any pred (cdr lst)))))"
    , ""
    , "(define (all pred lst)"
    , "  (cond ((null? lst) #t)"
    , "        ((not (pred (car lst))) #f)"
    , "        (else (all pred (cdr lst)))))"
    , ""
    , "(define (compose f g)"
    , "  (lambda (x) (f (g x))))"
    , ""
    , "(define (curry f x)"
    , "  (lambda args (apply f (cons x args))))"
    , ""
    , "(define (flip f)"
    , "  (lambda (x y) (f y x)))"
    , ""
    , "(define (id x) x)"
    , ""
    , "(define (const x)"
    , "  (lambda (_) x))"
    , ""
    , "(define (take n lst)"
    , "  (if (or (zero? n) (null? lst))"
    , "      '()"
    , "      (cons (car lst) (take (- n 1) (cdr lst)))))"
    , ""
    , "(define (drop n lst)"
    , "  (if (or (zero? n) (null? lst))"
    , "      lst"
    , "      (drop (- n 1) (cdr lst))))"
    , ""
    , "(define (last lst)"
    , "  (if (null? (cdr lst))"
    , "      (car lst)"
    , "      (last (cdr lst))))"
    , ""
    , "(define (init lst)"
    , "  (if (null? (cdr lst))"
    , "      '()"
    , "      (cons (car lst) (init (cdr lst)))))"
    , ""
    , "(define (zip lst1 lst2)"
    , "  (if (or (null? lst1) (null? lst2))"
    , "      '()"
    , "      (cons (list (car lst1) (car lst2))"
    , "            (zip (cdr lst1) (cdr lst2)))))"
    , ""
    , "(define (unzip lst)"
    , "  (list (map car lst) (map cadr lst)))"
    , ""
    , "(define (range start end)"
    , "  (if (>= start end)"
    , "      '()"
    , "      (cons start (range (+ start 1) end))))"
    , ""
    , "(define (iota n)"
    , "  (range 0 n))"
    , ""
    , "(define (sum lst)"
    , "  (fold-left + 0 lst))"
    , ""
    , "(define (product lst)"
    , "  (fold-left * 1 lst))"
    , ""
    , ";; Dynamic-wind (R5RS) - simplified version"
    , ";; Note: Full dynamic-wind with call/cc integration requires primitive support"
    , "(define (dynamic-wind before body after)"
    , "  (before)"
    , "  (let ((result (body)))"
    , "    (after)"
    , "    result))"
    , ""
    , ";; Promise (delay/force)"
    , "(define (make-promise proc)"
    , "  (let ((result-ready? #f)"
    , "        (result #f))"
    , "    (lambda ()"
    , "      (if result-ready?"
    , "          result"
    , "          (let ((x (proc)))"
    , "            (if result-ready?"
    , "                result"
    , "                (begin (set! result-ready? #t)"
    , "                       (set! result x)"
    , "                       result)))))))"
    , ""
    , ";; force - unwrap a promise"
    , "(define (force promise) (promise))"
    ]

-- | Create environment with primitives and stdlib
primitiveEnvWithStdlib :: IO Env
primitiveEnvWithStdlib = do
    env <- primitiveBindings
    loadStdlib env
    return env
  where
    loadStdlib env = do
        result <- runExceptT $ do
            exprs <- liftThrows $ readExprList stdlib
            mapM_ (eval env) exprs
        case result of
            Left err -> putStrLn $ "Error loading stdlib: " ++ show err
            Right _  -> return ()

-- | Run REPL
runRepl :: IO ()
runRepl = do
    putStrLn "R5RS Scheme Interpreter"
    putStrLn "Type (exit) or Ctrl-D to quit"
    putStrLn ""
    env <- primitiveEnvWithStdlib
    repl env

-- | REPL loop
repl :: Env -> IO ()
repl env = do
    putStr "scheme> "
    hFlush stdout
    eof <- isEOF
    unless eof $ do
        input <- getLine
        case input of
            "(exit)" -> return ()
            "(quit)" -> return ()
            ""       -> repl env
            _        -> do
                result <- evalString env input
                unless (null result) $ putStrLn result
                repl env

-- | Run a file
runFile :: String -> IO ()
runFile filename = do
    env <- primitiveEnvWithStdlib
    contents <- readFile filename
    result <- runExceptT $ do
        exprs <- liftThrows $ readExprList contents
        results <- mapM (eval env) exprs
        return $ last results
    case result of
        Left err  -> putStrLn $ "Error: " ++ show err
        Right val -> unless (val == Void) $ print val
