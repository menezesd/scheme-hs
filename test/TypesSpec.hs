{-# LANGUAGE ScopedTypeVariables #-}

module TypesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Types
import Data.Ratio ((%))
import Data.Complex (Complex(..), realPart, imagPart)

-- Arbitrary instance for SchemeNum
instance Arbitrary SchemeNum where
    arbitrary = oneof
        [ SInteger <$> arbitrary
        , do
            num <- arbitrary
            Positive denom <- arbitrary
            return $ SRational (num % denom)
        , SReal <$> arbitrary
        , do
            r <- arbitrary
            i <- arbitrary
            return $ SComplex (r :+ i)
        ]

spec :: Spec
spec = do
    describe "Types.SchemeNum" $ do
        describe "isExact" $ do
            it "integers are exact" $ do
                isExact (SInteger 42) `shouldBe` True
                isExact (SInteger 0) `shouldBe` True
                isExact (SInteger (-100)) `shouldBe` True

            it "rationals are exact" $ do
                isExact (SRational (1 % 2)) `shouldBe` True
                isExact (SRational ((-3) % 4)) `shouldBe` True

            it "reals are inexact" $ do
                isExact (SReal 3.14) `shouldBe` False
                isExact (SReal 0.0) `shouldBe` False

            it "complex with zero imaginary and integer real is exact" $ do
                isExact (SComplex (3.0 :+ 0)) `shouldBe` True
                isExact (SComplex ((-5.0) :+ 0)) `shouldBe` True

            it "complex with non-zero imaginary is inexact" $ do
                isExact (SComplex (1 :+ 1)) `shouldBe` False
                isExact (SComplex (0 :+ 1)) `shouldBe` False

        describe "schemeNumToDouble" $ do
            it "converts integers" $ do
                schemeNumToDouble (SInteger 42) `shouldBe` 42.0
                schemeNumToDouble (SInteger (-10)) `shouldBe` (-10.0)

            it "converts rationals" $ do
                schemeNumToDouble (SRational (1 % 2)) `shouldBe` 0.5
                schemeNumToDouble (SRational (3 % 4)) `shouldBe` 0.75

            it "converts reals" $ do
                schemeNumToDouble (SReal 3.14) `shouldBe` 3.14

            it "extracts real part from complex" $ do
                schemeNumToDouble (SComplex (3 :+ 4)) `shouldBe` 3.0

        describe "promoteToComplex" $ do
            it "promotes integers" $ do
                promoteToComplex (SInteger 42) `shouldBe` (42 :+ 0)

            it "promotes rationals" $ do
                promoteToComplex (SRational (1 % 2)) `shouldBe` (0.5 :+ 0)

            it "promotes reals" $ do
                promoteToComplex (SReal 3.14) `shouldBe` (3.14 :+ 0)

            it "keeps complex as is" $ do
                promoteToComplex (SComplex (3 :+ 4)) `shouldBe` (3 :+ 4)

        describe "simplifyNum" $ do
            it "simplifies complex with zero imaginary to real" $ do
                simplifyNum (SComplex (5 :+ 0)) `shouldBe` SInteger 5
                simplifyNum (SComplex (3.14 :+ 0)) `shouldBe` SReal 3.14

            it "simplifies real integers to integer" $ do
                simplifyNum (SReal 5.0) `shouldBe` SInteger 5
                simplifyNum (SReal (-10.0)) `shouldBe` SInteger (-10)

            it "keeps non-integer reals" $ do
                simplifyNum (SReal 3.14) `shouldBe` SReal 3.14

            it "simplifies rational with denominator 1 to integer" $ do
                simplifyNum (SRational (5 % 1)) `shouldBe` SInteger 5
                simplifyNum (SRational ((-3) % 1)) `shouldBe` SInteger (-3)

            it "keeps proper rationals" $ do
                simplifyNum (SRational (1 % 2)) `shouldBe` SRational (1 % 2)

            it "keeps integers as is" $ do
                simplifyNum (SInteger 42) `shouldBe` SInteger 42

        describe "showSchemeNum" $ do
            it "shows integers" $ do
                show (SInteger 42) `shouldBe` "42"
                show (SInteger (-10)) `shouldBe` "-10"

            it "shows rationals" $ do
                show (SRational (1 % 2)) `shouldBe` "1/2"
                show (SRational (3 % 4)) `shouldBe` "3/4"

            it "shows rationals with denominator 1 as integers" $ do
                show (SRational (5 % 1)) `shouldBe` "5"

            it "shows reals" $ do
                show (SReal 3.5) `shouldBe` "3.5"
                show (SReal 42.0) `shouldBe` "42.0"

            it "shows complex numbers" $ do
                show (SComplex (3 :+ 4)) `shouldBe` "3.0+4.0i"
                show (SComplex (3 :+ (-4))) `shouldBe` "3.0-4.0i"

            it "shows pure imaginary" $ do
                show (SComplex (0 :+ 1)) `shouldBe` "i"
                show (SComplex (0 :+ (-1))) `shouldBe` "-i"

            it "shows special values" $ do
                show (SReal (1/0)) `shouldBe` "+inf.0"
                show (SReal ((-1)/0)) `shouldBe` "-inf.0"
                show (SReal (0/0)) `shouldBe` "+nan.0"

    describe "Types.LispVal" $ do
        describe "showVal" $ do
            it "shows atoms" $ do
                showVal (Atom "foo") `shouldBe` "foo"
                showVal (Atom "+") `shouldBe` "+"

            it "shows lists" $ do
                showVal (List []) `shouldBe` "()"
                showVal (List [Atom "a", Atom "b"]) `shouldBe` "(a b)"

            it "shows dotted lists" $ do
                showVal (DottedList [Atom "a"] (Atom "b")) `shouldBe` "(a . b)"

            it "shows strings" $ do
                showVal (String "hello") `shouldBe` "\"hello\""
                showVal (String "line\nbreak") `shouldBe` "\"line\\nbreak\""

            it "shows characters" $ do
                showVal (Char 'a') `shouldBe` "#\\a"
                showVal (Char ' ') `shouldBe` "#\\space"
                showVal (Char '\n') `shouldBe` "#\\newline"

            it "shows booleans" $ do
                showVal (Bool True) `shouldBe` "#t"
                showVal (Bool False) `shouldBe` "#f"

            it "shows numbers" $ do
                showVal (Number (SInteger 42)) `shouldBe` "42"
                showVal (Number (SRational (1 % 2))) `shouldBe` "1/2"

            it "shows EOF" $ do
                showVal EOF `shouldBe` "#<eof>"

            it "shows Void as empty" $ do
                showVal Void `shouldBe` ""

        describe "Eq instance" $ do
            it "atoms equal when same name" $ do
                Atom "foo" `shouldBe` Atom "foo"
                Atom "foo" `shouldNotBe` Atom "bar"

            it "numbers equal when same value" $ do
                Number (SInteger 42) `shouldBe` Number (SInteger 42)
                Number (SInteger 42) `shouldNotBe` Number (SInteger 43)

            it "lists equal when same elements" $ do
                List [Atom "a", Atom "b"] `shouldBe` List [Atom "a", Atom "b"]
                List [Atom "a"] `shouldNotBe` List [Atom "b"]

            it "booleans equal when same value" $ do
                Bool True `shouldBe` Bool True
                Bool False `shouldBe` Bool False
                Bool True `shouldNotBe` Bool False

    describe "Types.LispError" $ do
        describe "showError" $ do
            it "shows NumArgs error" $ do
                showError (NumArgs 2 [Number (SInteger 1)]) `shouldBe`
                    "Expected 2 args; found values 1"

            it "shows TypeMismatch error" $ do
                showError (TypeMismatch "number" (String "hello")) `shouldBe`
                    "Invalid type: expected number, found \"hello\""

            it "shows DivideByZero error" $ do
                showError DivideByZero `shouldBe` "Division by zero"

            it "shows UnboundVar error" $ do
                showError (UnboundVar "Unbound variable" "x") `shouldBe`
                    "Unbound variable: x"

            it "shows Default error" $ do
                showError (Default "Something went wrong") `shouldBe`
                    "Something went wrong"

    -- Property-based tests
    describe "SchemeNum properties" $ do
        it "simplifyNum is idempotent" $ property $
            \(n :: SchemeNum) -> simplifyNum (simplifyNum n) == simplifyNum n

        it "promoteToComplex preserves real part for reals" $ property $
            \(d :: Double) ->
                not (isNaN d) ==>
                    let c = promoteToComplex (SReal d)
                    in realPart c == d && imagPart c == 0

        it "schemeNumToDouble of integer matches fromIntegral" $ property $
            \(n :: Integer) ->
                schemeNumToDouble (SInteger n) == fromIntegral n

        it "promoteToComplex of integer has zero imaginary part" $ property $
            \(n :: Integer) ->
                imagPart (promoteToComplex (SInteger n)) == 0

        it "SInteger equality is reflexive" $ property $
            \(n :: Integer) ->
                SInteger n == SInteger n

        it "SReal equality is reflexive for non-NaN" $ property $
            \(d :: Double) ->
                not (isNaN d) ==> SReal d == SReal d

        it "simplifyNum preserves value" $ property $
            \(n :: SchemeNum) ->
                let simplified = simplifyNum n
                    originalDouble = schemeNumToDouble n
                    simplifiedDouble = schemeNumToDouble simplified
                in if isNaN originalDouble
                   then isNaN simplifiedDouble
                   else abs (originalDouble - simplifiedDouble) < 1e-10

        it "isExact is false for SReal" $ property $
            \(d :: Double) ->
                not (isExact (SReal d))

        it "isExact is true for SInteger" $ property $
            \(n :: Integer) ->
                isExact (SInteger n)
