-- | Benchmark suite for the Scheme interpreter
--
-- This module provides performance benchmarks for:
--
-- * Parser performance
-- * Evaluator performance
-- * Numeric operations
-- * List operations
-- * String operations
module Main where

import Criterion.Main
import Control.Monad.Except (runExceptT)
import Control.DeepSeq
import Data.Either (fromRight)

import Types
import Parser
import Eval
import Primitives

-- | Make LispVal deeply evaluatable for benchmarking
instance NFData LispVal where
    rnf (Atom s) = rnf s
    rnf (List xs) = rnf xs
    rnf (DottedList xs x) = rnf xs `seq` rnf x
    rnf (Number n) = rnf n
    rnf (String s) = rnf s
    rnf (Char c) = rnf c
    rnf (Bool b) = rnf b
    rnf (Vector _) = ()  -- Arrays don't have NFData
    rnf (PrimitiveFunc _) = ()
    rnf (IOFunc _) = ()
    rnf (Port _) = ()
    rnf EOF = ()
    rnf (Func _ _ _ _) = ()
    rnf (Macro _ _ _) = ()
    rnf (Syntax _) = ()
    rnf (Cont _) = ()
    rnf Void = ()
    rnf (MutablePair _ _) = ()

instance NFData SchemeNum where
    rnf (SInteger n) = rnf n
    rnf (SRational r) = rnf r
    rnf (SReal d) = rnf d
    rnf (SComplex c) = rnf c

-- | Parse an expression (unsafe, for benchmarking)
unsafeParse :: String -> LispVal
unsafeParse s = fromRight (Atom "error") (readExpr s)

-- | Evaluate an expression in a fresh environment
evalFresh :: String -> IO LispVal
evalFresh expr = do
    env <- primitiveBindings
    result <- runExceptT $ eval env (unsafeParse expr)
    return $ fromRight Void result

-- | Benchmark a Scheme expression
benchScheme :: String -> String -> Benchmark
benchScheme name expr = bench name $ nfIO $ evalFresh expr

main :: IO ()
main = defaultMain
    [ bgroup "parser"
        [ bench "simple atom" $ nf unsafeParse "hello"
        , bench "simple number" $ nf unsafeParse "42"
        , bench "float" $ nf unsafeParse "3.14159"
        , bench "complex" $ nf unsafeParse "3+4i"
        , bench "rational" $ nf unsafeParse "22/7"
        , bench "simple list" $ nf unsafeParse "(1 2 3 4 5)"
        , bench "nested list" $ nf unsafeParse "((1 2) (3 4) (5 6))"
        , bench "quoted" $ nf unsafeParse "'(a b c)"
        , bench "string" $ nf unsafeParse "\"hello world\""
        , bench "complex expression" $ nf unsafeParse
            "(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"
        , bench "large list" $ nf unsafeParse $
            "(" ++ unwords (map show [1..100 :: Int]) ++ ")"
        ]

    , bgroup "arithmetic"
        [ benchScheme "addition 2 args" "(+ 1 2)"
        , benchScheme "addition 10 args" "(+ 1 2 3 4 5 6 7 8 9 10)"
        , benchScheme "multiplication" "(* 2 3 4 5)"
        , benchScheme "division" "(/ 100 2 5)"
        , benchScheme "mixed ops" "(+ (* 2 3) (/ 10 2) (- 5 3))"
        , benchScheme "exponentiation" "(expt 2 10)"
        , benchScheme "sqrt" "(sqrt 144)"
        , benchScheme "gcd" "(gcd 48 18)"
        , benchScheme "rational arithmetic" "(+ 1/2 1/3 1/4)"
        ]

    , bgroup "comparison"
        [ benchScheme "numeric eq" "(= 1 1)"
        , benchScheme "less than" "(< 1 2 3 4 5)"
        , benchScheme "greater than" "(> 5 4 3 2 1)"
        ]

    , bgroup "list operations"
        [ benchScheme "cons" "(cons 1 '(2 3 4))"
        , benchScheme "car" "(car '(1 2 3 4 5))"
        , benchScheme "cdr" "(cdr '(1 2 3 4 5))"
        , benchScheme "length 10" "(length '(1 2 3 4 5 6 7 8 9 10))"
        , benchScheme "append" "(append '(1 2 3) '(4 5 6))"
        , benchScheme "reverse 10" "(reverse '(1 2 3 4 5 6 7 8 9 10))"
        , benchScheme "list-ref" "(list-ref '(a b c d e) 2)"
        , benchScheme "member" "(member 'c '(a b c d e))"
        ]

    , bgroup "string operations"
        [ benchScheme "string-length" "(string-length \"hello world\")"
        , benchScheme "string-append" "(string-append \"hello\" \" \" \"world\")"
        , benchScheme "substring" "(substring \"hello world\" 0 5)"
        , benchScheme "string=?" "(string=? \"hello\" \"hello\")"
        ]

    , bgroup "predicates"
        [ benchScheme "number?" "(number? 42)"
        , benchScheme "string?" "(string? \"hello\")"
        , benchScheme "list?" "(list? '(1 2 3))"
        , benchScheme "null?" "(null? '())"
        , benchScheme "pair?" "(pair? '(1 . 2))"
        ]

    , bgroup "control flow"
        [ benchScheme "if true" "(if #t 1 2)"
        , benchScheme "if false" "(if #f 1 2)"
        , benchScheme "cond" "(cond ((= 1 2) 'a) ((= 2 2) 'b) (else 'c))"
        , benchScheme "and short-circuit" "(and #f (error \"never\"))"
        , benchScheme "or short-circuit" "(or #t (error \"never\"))"
        ]

    , bgroup "lambda"
        [ benchScheme "simple lambda" "((lambda (x) x) 42)"
        , benchScheme "lambda with closure"
            "(let ((y 10)) ((lambda (x) (+ x y)) 5))"
        , benchScheme "higher-order" "(map (lambda (x) (* x 2)) '(1 2 3 4 5))"
        ]

    , bgroup "recursion"
        [ benchScheme "factorial 10"
            "(letrec ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 10))"
        , benchScheme "fibonacci 10"
            "(letrec ((fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 10))"
        , benchScheme "fold-left" "(fold-left + 0 '(1 2 3 4 5 6 7 8 9 10))"
        ]

    , bgroup "vectors"
        [ benchScheme "make-vector" "(make-vector 100 0)"
        , benchScheme "vector-ref" "(vector-ref #(1 2 3 4 5) 2)"
        , benchScheme "vector->list" "(vector->list #(1 2 3 4 5))"
        ]

    , bgroup "equality"
        [ benchScheme "eq? atoms" "(eq? 'a 'a)"
        , benchScheme "eqv? numbers" "(eqv? 42 42)"
        , benchScheme "equal? lists" "(equal? '(1 2 3) '(1 2 3))"
        , benchScheme "equal? nested" "(equal? '((1 2) (3 4)) '((1 2) (3 4)))"
        ]
    ]
