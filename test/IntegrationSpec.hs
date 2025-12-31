{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Except
import Data.Ratio ((%))
import Parser
import Types
import Eval
import Primitives
import Env

-- Helper to evaluate a Scheme expression and get the result
evalScheme :: String -> IO (Either LispError LispVal)
evalScheme expr = do
    env <- primitiveBindings
    runExceptT $ liftThrows (readExpr expr) >>= eval env

-- Helper to evaluate and expect success
evalOk :: String -> IO LispVal
evalOk expr = do
    result <- evalScheme expr
    case result of
        Right v -> return v
        Left e  -> error $ "Eval failed: " ++ show e

-- Helper to evaluate and expect a specific result
shouldEvalTo :: String -> LispVal -> Expectation
shouldEvalTo expr expected = do
    result <- evalOk expr
    result `shouldBe` expected

-- Helper to evaluate and expect failure
shouldFail :: String -> Expectation
shouldFail expr = do
    result <- evalScheme expr
    case result of
        Left _ -> return ()
        Right v -> expectationFailure $ "Expected failure, got: " ++ show v

spec :: Spec
spec = do
    describe "Arithmetic" $ do
        it "adds integers" $ do
            "(+ 1 2 3)" `shouldEvalTo` Number (SInteger 6)

        it "subtracts integers" $ do
            "(- 10 3)" `shouldEvalTo` Number (SInteger 7)
            "(- 5)" `shouldEvalTo` Number (SInteger (-5))

        it "multiplies integers" $ do
            "(* 2 3 4)" `shouldEvalTo` Number (SInteger 24)

        it "divides integers" $ do
            "(/ 10 2)" `shouldEvalTo` Number (SInteger 5)
            "(/ 1 2)" `shouldEvalTo` Number (SRational (1 % 2))

        it "handles rational arithmetic" $ do
            "(+ 1/2 1/3)" `shouldEvalTo` Number (SRational (5 % 6))
            "(* 2/3 3/4)" `shouldEvalTo` Number (SRational (1 % 2))

        it "handles mixed numeric types" $ do
            "(+ 1 1/2)" `shouldEvalTo` Number (SRational (3 % 2))

        it "handles negative numbers" $ do
            "(+ -1 -2)" `shouldEvalTo` Number (SInteger (-3))
            "(* -2 3)" `shouldEvalTo` Number (SInteger (-6))

    describe "Comparison" $ do
        it "compares numbers with =" $ do
            "(= 1 1)" `shouldEvalTo` Bool True
            "(= 1 2)" `shouldEvalTo` Bool False
            "(= 1 1 1)" `shouldEvalTo` Bool True

        it "compares numbers with <" $ do
            "(< 1 2)" `shouldEvalTo` Bool True
            "(< 2 1)" `shouldEvalTo` Bool False
            "(< 1 2 3)" `shouldEvalTo` Bool True

        it "compares numbers with >" $ do
            "(> 2 1)" `shouldEvalTo` Bool True
            "(> 1 2)" `shouldEvalTo` Bool False

    describe "Boolean operations" $ do
        it "not works" $ do
            "(not #f)" `shouldEvalTo` Bool True
            "(not #t)" `shouldEvalTo` Bool False
            "(not 1)" `shouldEvalTo` Bool False

        it "and works" $ do
            "(and #t #t)" `shouldEvalTo` Bool True
            "(and #t #f)" `shouldEvalTo` Bool False
            "(and)" `shouldEvalTo` Bool True

        it "or works" $ do
            "(or #f #t)" `shouldEvalTo` Bool True
            "(or #f #f)" `shouldEvalTo` Bool False
            "(or)" `shouldEvalTo` Bool False

    describe "List operations" $ do
        it "car gets first element" $ do
            "(car '(1 2 3))" `shouldEvalTo` Number (SInteger 1)

        it "cdr gets rest" $ do
            "(cdr '(1 2 3))" `shouldEvalTo` List [Number (SInteger 2), Number (SInteger 3)]

        it "cons builds pairs" $ do
            "(cons 1 '())" `shouldEvalTo` List [Number (SInteger 1)]
            "(cons 1 '(2 3))" `shouldEvalTo` List [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3)]

        it "list builds lists" $ do
            "(list 1 2 3)" `shouldEvalTo` List [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3)]

        it "length works" $ do
            "(length '(1 2 3))" `shouldEvalTo` Number (SInteger 3)
            "(length '())" `shouldEvalTo` Number (SInteger 0)

        it "append works" $ do
            "(append '(1 2) '(3 4))" `shouldEvalTo` List [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3), Number (SInteger 4)]

        it "reverse works" $ do
            "(reverse '(1 2 3))" `shouldEvalTo` List [Number (SInteger 3), Number (SInteger 2), Number (SInteger 1)]

    describe "Type predicates" $ do
        it "number? works" $ do
            "(number? 42)" `shouldEvalTo` Bool True
            "(number? \"hello\")" `shouldEvalTo` Bool False

        it "string? works" $ do
            "(string? \"hello\")" `shouldEvalTo` Bool True
            "(string? 42)" `shouldEvalTo` Bool False

        it "symbol? works" $ do
            "(symbol? 'foo)" `shouldEvalTo` Bool True
            "(symbol? 42)" `shouldEvalTo` Bool False

        it "pair? works" $ do
            "(pair? '(1 2))" `shouldEvalTo` Bool True
            "(pair? '())" `shouldEvalTo` Bool False

        it "null? works" $ do
            "(null? '())" `shouldEvalTo` Bool True
            "(null? '(1))" `shouldEvalTo` Bool False

    describe "String operations" $ do
        it "string-length works" $ do
            "(string-length \"hello\")" `shouldEvalTo` Number (SInteger 5)

        it "string-append works" $ do
            "(string-append \"hello\" \" \" \"world\")" `shouldEvalTo` String "hello world"

        it "string comparisons work" $ do
            "(string=? \"abc\" \"abc\")" `shouldEvalTo` Bool True
            "(string<? \"abc\" \"abd\")" `shouldEvalTo` Bool True

    describe "Lambda and define" $ do
        it "lambda creates functions" $ do
            "((lambda (x) (* x x)) 5)" `shouldEvalTo` Number (SInteger 25)

        it "lambda with multiple args" $ do
            "((lambda (x y) (+ x y)) 3 4)" `shouldEvalTo` Number (SInteger 7)

        it "lambda with varargs" $ do
            "((lambda args (length args)) 1 2 3)" `shouldEvalTo` Number (SInteger 3)

    describe "Let forms" $ do
        it "let binds variables" $ do
            "(let ((x 1) (y 2)) (+ x y))" `shouldEvalTo` Number (SInteger 3)

        it "let* allows sequential bindings" $ do
            "(let* ((x 1) (y (+ x 1))) (+ x y))" `shouldEvalTo` Number (SInteger 3)

        it "letrec allows recursive bindings" $ do
            "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))" `shouldEvalTo` Number (SInteger 120)

        it "named let works" $ do
            "(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))" `shouldEvalTo` Number (SInteger 120)

    describe "Conditionals" $ do
        it "if with true condition" $ do
            "(if #t 1 2)" `shouldEvalTo` Number (SInteger 1)

        it "if with false condition" $ do
            "(if #f 1 2)" `shouldEvalTo` Number (SInteger 2)

        it "cond works" $ do
            "(cond (#f 1) (#t 2) (else 3))" `shouldEvalTo` Number (SInteger 2)
            "(cond (#f 1) (#f 2) (else 3))" `shouldEvalTo` Number (SInteger 3)

        it "case works" $ do
            "(case 2 ((1) 'one) ((2) 'two) (else 'other))" `shouldEvalTo` Atom "two"

    describe "Quotation" $ do
        it "quote returns data" $ do
            "'(1 2 3)" `shouldEvalTo` List [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3)]
            "'foo" `shouldEvalTo` Atom "foo"

        it "quasiquote with unquote" $ do
            result <- evalOk "(let ((x 5)) `(1 ,x 3))"
            result `shouldBe` List [Number (SInteger 1), Number (SInteger 5), Number (SInteger 3)]

    describe "Higher-order functions" $ do
        it "map works" $ do
            "(map (lambda (x) (* x 2)) '(1 2 3))" `shouldEvalTo` List [Number (SInteger 2), Number (SInteger 4), Number (SInteger 6)]

        it "apply works" $ do
            "(apply + '(1 2 3))" `shouldEvalTo` Number (SInteger 6)
            "(apply + 1 2 '(3 4))" `shouldEvalTo` Number (SInteger 10)

    describe "Numeric predicates" $ do
        it "zero? works" $ do
            "(zero? 0)" `shouldEvalTo` Bool True
            "(zero? 1)" `shouldEvalTo` Bool False

        it "positive? works" $ do
            "(positive? 1)" `shouldEvalTo` Bool True
            "(positive? -1)" `shouldEvalTo` Bool False

        it "negative? works" $ do
            "(negative? -1)" `shouldEvalTo` Bool True
            "(negative? 1)" `shouldEvalTo` Bool False

        it "odd? and even? work" $ do
            "(odd? 3)" `shouldEvalTo` Bool True
            "(even? 4)" `shouldEvalTo` Bool True

        it "exact? and inexact? work" $ do
            "(exact? 42)" `shouldEvalTo` Bool True
            "(inexact? 3.14)" `shouldEvalTo` Bool True

    describe "Math functions" $ do
        it "abs works" $ do
            "(abs -5)" `shouldEvalTo` Number (SInteger 5)
            "(abs 5)" `shouldEvalTo` Number (SInteger 5)

        it "max and min work" $ do
            "(max 1 2 3)" `shouldEvalTo` Number (SInteger 3)
            "(min 1 2 3)" `shouldEvalTo` Number (SInteger 1)

        it "quotient, remainder, modulo work" $ do
            "(quotient 10 3)" `shouldEvalTo` Number (SInteger 3)
            "(remainder 10 3)" `shouldEvalTo` Number (SInteger 1)
            "(modulo 10 3)" `shouldEvalTo` Number (SInteger 1)

        it "gcd and lcm work" $ do
            "(gcd 12 8)" `shouldEvalTo` Number (SInteger 4)
            "(lcm 3 4)" `shouldEvalTo` Number (SInteger 12)

        it "expt works" $ do
            "(expt 2 10)" `shouldEvalTo` Number (SInteger 1024)
            "(expt 2 -1)" `shouldEvalTo` Number (SRational (1 % 2))

        it "floor, ceiling, truncate, round work" $ do
            "(floor 3.7)" `shouldEvalTo` Number (SInteger 3)
            "(ceiling 3.2)" `shouldEvalTo` Number (SInteger 4)
            "(truncate -3.7)" `shouldEvalTo` Number (SInteger (-3))
            "(round 3.5)" `shouldEvalTo` Number (SInteger 4)

    describe "Equivalence predicates" $ do
        it "eq? works" $ do
            "(eq? 'a 'a)" `shouldEvalTo` Bool True
            "(eq? 'a 'b)" `shouldEvalTo` Bool False
            "(eq? 1 1)" `shouldEvalTo` Bool True

        it "eqv? works" $ do
            "(eqv? #t #t)" `shouldEvalTo` Bool True
            "(eqv? 100 100)" `shouldEvalTo` Bool True

        it "equal? works on lists" $ do
            "(equal? '(1 2 3) '(1 2 3))" `shouldEvalTo` Bool True
            "(equal? '(1 2 3) '(1 2 4))" `shouldEvalTo` Bool False

    describe "Vectors" $ do
        it "vector creates vectors" $ do
            result <- evalOk "(vector 1 2 3)"
            case result of
                Vector _ -> return ()
                _ -> expectationFailure "Expected vector"

        it "vector-ref works" $ do
            "(vector-ref #(1 2 3) 0)" `shouldEvalTo` Number (SInteger 1)
            "(vector-ref #(1 2 3) 2)" `shouldEvalTo` Number (SInteger 3)

        it "vector-length works" $ do
            "(vector-length #(1 2 3))" `shouldEvalTo` Number (SInteger 3)

    describe "Characters" $ do
        it "char comparisons work" $ do
            "(char=? #\\a #\\a)" `shouldEvalTo` Bool True
            "(char<? #\\a #\\b)" `shouldEvalTo` Bool True

        it "char->integer and integer->char work" $ do
            "(char->integer #\\A)" `shouldEvalTo` Number (SInteger 65)
            "(integer->char 65)" `shouldEvalTo` Char 'A'

        it "char predicates work" $ do
            "(char-alphabetic? #\\a)" `shouldEvalTo` Bool True
            "(char-numeric? #\\5)" `shouldEvalTo` Bool True
            "(char-whitespace? #\\space)" `shouldEvalTo` Bool True

    describe "Error handling" $ do
        it "division by zero fails" $ shouldFail "(/ 1 0)"
        it "unbound variable fails" $ shouldFail "undefined-variable"
        it "wrong number of args fails" $ shouldFail "(+ 1 2 . 3)"

    describe "Complex numbers" $ do
        it "sqrt of negative" $ do
            result <- evalOk "(sqrt -4)"
            case result of
                Number (SComplex _) -> return ()
                _ -> expectationFailure "Expected complex number"

        it "make-rectangular works" $ do
            result <- evalOk "(make-rectangular 3 4)"
            case result of
                Number (SComplex _) -> return ()
                _ -> expectationFailure "Expected complex number"

    describe "Call/cc" $ do
        it "escape continuation works" $ do
            "(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))" `shouldEvalTo` Number (SInteger 4)

        it "call/cc allows early return" $ do
            result <- evalOk "(call/cc (lambda (return) (return 42) 0))"
            result `shouldBe` Number (SInteger 42)

    describe "Do loop" $ do
        it "do loop with iteration" $ do
            "(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum))" `shouldEvalTo` Number (SInteger 10)

    describe "Begin" $ do
        it "begin evaluates expressions in order" $ do
            "(begin 1 2 3)" `shouldEvalTo` Number (SInteger 3)

    describe "Property tests" $ do
        it "arithmetic identity: (+ n 0) = n" $ property $
            \(n :: Integer) -> ioProperty $ do
                result <- evalOk $ "(+ " ++ show n ++ " 0)"
                return $ result == Number (SInteger n)

        it "multiplication identity: (* n 1) = n" $ property $
            \(n :: Integer) -> ioProperty $ do
                result <- evalOk $ "(* " ++ show n ++ " 1)"
                return $ result == Number (SInteger n)

        it "negation is involution: (- (- n)) = n" $ property $
            \(n :: Integer) -> ioProperty $ do
                result <- evalOk $ "(- (- " ++ show n ++ "))"
                return $ result == Number (SInteger n)

        it "list length matches" $ property $
            \(Small (n :: Int)) -> n >= 0 && n <= 50 ==> ioProperty $ do
                let listStr = "(list " ++ unwords (replicate n "1") ++ ")"
                result <- evalOk $ "(length " ++ listStr ++ ")"
                return $ result == Number (SInteger (fromIntegral n))
