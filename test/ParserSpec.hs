{-# LANGUAGE ScopedTypeVariables #-}

module ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parser
import Types
import Data.Ratio ((%))
import Data.Complex (Complex(..))

-- Helper to extract Right value or fail
parseOk :: String -> LispVal
parseOk s = case readExpr s of
    Right v -> v
    Left e  -> error $ "Parse failed: " ++ show e

-- Helper to check if parsing fails
parseFails :: String -> Bool
parseFails s = case readExpr s of
    Left _  -> True
    Right _ -> False

spec :: Spec
spec = do
    describe "Parser.readExpr" $ do
        -- Atoms
        describe "atoms" $ do
            it "parses simple atoms" $ do
                parseOk "foo" `shouldBe` Atom "foo"
                parseOk "bar" `shouldBe` Atom "bar"
                parseOk "+" `shouldBe` Atom "+"
                parseOk "-" `shouldBe` Atom "-"

            it "parses atoms with special characters" $ do
                parseOk "foo-bar" `shouldBe` Atom "foo-bar"
                parseOk "foo?" `shouldBe` Atom "foo?"
                parseOk "foo!" `shouldBe` Atom "foo!"
                parseOk "->string" `shouldBe` Atom "->string"

        -- Integers
        describe "integers" $ do
            it "parses positive integers" $ do
                parseOk "42" `shouldBe` Number (SInteger 42)
                parseOk "0" `shouldBe` Number (SInteger 0)
                parseOk "123456789" `shouldBe` Number (SInteger 123456789)

            it "parses negative integers" $ do
                parseOk "-42" `shouldBe` Number (SInteger (-42))
                parseOk "-1" `shouldBe` Number (SInteger (-1))

            it "parses explicitly positive integers" $ do
                parseOk "+42" `shouldBe` Number (SInteger 42)

        -- Rationals
        describe "rationals" $ do
            it "parses simple rationals" $ do
                parseOk "1/2" `shouldBe` Number (SRational (1 % 2))
                parseOk "3/4" `shouldBe` Number (SRational (3 % 4))

            it "simplifies rationals" $ do
                parseOk "2/4" `shouldBe` Number (SRational (1 % 2))
                parseOk "6/3" `shouldBe` Number (SInteger 2)

            it "parses negative rationals" $ do
                parseOk "-1/2" `shouldBe` Number (SRational ((-1) % 2))

        -- Floats
        describe "floats" $ do
            it "parses simple floats" $ do
                parseOk "3.14" `shouldBe` Number (SReal 3.14)
                parseOk "0.5" `shouldBe` Number (SReal 0.5)
                parseOk ".5" `shouldBe` Number (SReal 0.5)

            it "parses negative floats" $ do
                parseOk "-3.14" `shouldBe` Number (SReal (-3.14))

            it "parses floats with exponents" $ do
                parseOk "1.5e2" `shouldBe` Number (SReal 150.0)
                parseOk "1.5e-2" `shouldBe` Number (SReal 0.015)

        -- Complex numbers
        describe "complex numbers" $ do
            it "parses simple complex" $ do
                parseOk "1+2i" `shouldBe` Number (SComplex (1 :+ 2))
                parseOk "3-4i" `shouldBe` Number (SComplex (3 :+ (-4)))

            it "parses pure imaginary" $ do
                parseOk "0+1i" `shouldBe` Number (SComplex (0 :+ 1))
                parseOk "0-1i" `shouldBe` Number (SComplex (0 :+ (-1)))

        -- Radix numbers
        describe "radix numbers" $ do
            it "parses hexadecimal" $ do
                parseOk "#xff" `shouldBe` Number (SInteger 255)
                parseOk "#xFF" `shouldBe` Number (SInteger 255)
                parseOk "#x10" `shouldBe` Number (SInteger 16)

            it "parses octal" $ do
                parseOk "#o77" `shouldBe` Number (SInteger 63)
                parseOk "#o10" `shouldBe` Number (SInteger 8)

            it "parses binary" $ do
                parseOk "#b101" `shouldBe` Number (SInteger 5)
                parseOk "#b1111" `shouldBe` Number (SInteger 15)

            it "parses with exactness prefix" $ do
                parseOk "#e10" `shouldBe` Number (SInteger 10)
                parseOk "#i10" `shouldBe` Number (SReal 10.0)

        -- Strings
        describe "strings" $ do
            it "parses simple strings" $ do
                parseOk "\"hello\"" `shouldBe` String "hello"
                parseOk "\"\"" `shouldBe` String ""

            it "parses strings with escapes" $ do
                parseOk "\"hello\\nworld\"" `shouldBe` String "hello\nworld"
                parseOk "\"tab\\there\"" `shouldBe` String "tab\there"
                parseOk "\"quote\\\"here\"" `shouldBe` String "quote\"here"

        -- Characters
        describe "characters" $ do
            it "parses simple characters" $ do
                parseOk "#\\a" `shouldBe` Char 'a'
                parseOk "#\\Z" `shouldBe` Char 'Z'
                parseOk "#\\5" `shouldBe` Char '5'

            it "parses named characters" $ do
                parseOk "#\\space" `shouldBe` Char ' '
                parseOk "#\\newline" `shouldBe` Char '\n'
                parseOk "#\\tab" `shouldBe` Char '\t'

        -- Booleans
        describe "booleans" $ do
            it "parses true" $ do
                parseOk "#t" `shouldBe` Bool True
                parseOk "#T" `shouldBe` Bool True

            it "parses false" $ do
                parseOk "#f" `shouldBe` Bool False
                parseOk "#F" `shouldBe` Bool False

        -- Lists
        describe "lists" $ do
            it "parses empty list" $ do
                parseOk "()" `shouldBe` List []

            it "parses simple list" $ do
                parseOk "(1 2 3)" `shouldBe` List [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3)]

            it "parses nested list" $ do
                parseOk "((1 2) 3)" `shouldBe` List [List [Number (SInteger 1), Number (SInteger 2)], Number (SInteger 3)]

            it "parses dotted list" $ do
                parseOk "(1 . 2)" `shouldBe` DottedList [Number (SInteger 1)] (Number (SInteger 2))
                parseOk "(1 2 . 3)" `shouldBe` DottedList [Number (SInteger 1), Number (SInteger 2)] (Number (SInteger 3))

        -- Quoted expressions
        describe "quoted expressions" $ do
            it "parses quote" $ do
                parseOk "'x" `shouldBe` List [Atom "quote", Atom "x"]
                parseOk "'(1 2 3)" `shouldBe` List [Atom "quote", List [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3)]]

            it "parses quasiquote" $ do
                parseOk "`x" `shouldBe` List [Atom "quasiquote", Atom "x"]

            it "parses unquote" $ do
                parseOk ",x" `shouldBe` List [Atom "unquote", Atom "x"]

            it "parses unquote-splicing" $ do
                parseOk ",@x" `shouldBe` List [Atom "unquote-splicing", Atom "x"]

        -- Vectors
        describe "vectors" $ do
            it "parses empty vector" $ do
                case parseOk "#()" of
                    Vector arr -> length (foldr (:) [] arr) `shouldBe` 0
                    _ -> expectationFailure "Expected vector"

            it "parses simple vector" $ do
                case parseOk "#(1 2 3)" of
                    Vector arr -> foldr (:) [] arr `shouldBe` [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3)]
                    _ -> expectationFailure "Expected vector"

        -- Comments
        describe "comments" $ do
            it "ignores line comments" $ do
                parseOk "; comment\n42" `shouldBe` Number (SInteger 42)
                parseOk "42 ; trailing comment" `shouldBe` Number (SInteger 42)

        -- Whitespace handling
        describe "whitespace" $ do
            it "handles various whitespace" $ do
                parseOk "  42  " `shouldBe` Number (SInteger 42)
                parseOk "\t42\n" `shouldBe` Number (SInteger 42)

    describe "Parser.readExprList" $ do
        it "parses multiple expressions" $ do
            case readExprList "1 2 3" of
                Right vals -> vals `shouldBe` [Number (SInteger 1), Number (SInteger 2), Number (SInteger 3)]
                Left e -> expectationFailure $ show e

        it "parses empty input" $ do
            case readExprList "" of
                Right vals -> vals `shouldBe` []
                Left e -> expectationFailure $ show e

    -- Property-based tests
    describe "Parser properties" $ do
        it "integers roundtrip" $ property $
            \(n :: Integer) ->
                let s = if n >= 0 then show n else show n
                in parseOk s == Number (SInteger n)

        it "positive integers roundtrip through parser" $ property $
            \(Positive (n :: Integer)) ->
                parseOk (show n) == Number (SInteger n)

        it "parsing preserves rational simplification" $ property $
            \(Positive (num :: Integer)) (Positive (denom :: Integer)) ->
                denom /= 0 ==>
                    let input = show num ++ "/" ++ show denom
                        result = parseOk input
                    in case result of
                        Number (SRational r) -> r == (num % denom)
                        Number (SInteger n) -> n == (num `div` denom) && num `mod` denom == 0
                        _ -> False

        it "list length preserved" $ property $
            \(Small (n :: Int)) ->
                n >= 0 && n <= 100 ==>
                    let input = "(" ++ unwords (replicate n "1") ++ ")"
                        result = parseOk input
                    in case result of
                        List elems -> length elems == n
                        _ -> n == 0  -- empty list case
