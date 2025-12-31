{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Fuzzing tests for the Scheme interpreter
--
-- This module provides property-based tests using QuickCheck to fuzz
-- the parser and evaluator, looking for crashes and unexpected behavior.
module FuzzSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Except (runExceptT)
import Data.Ratio ((%))
import Data.Complex (Complex((:+)))
import qualified Data.Ratio
import qualified Data.Complex

import Types
import Parser
import Eval
import Primitives

-- | Arbitrary instance for generating random SchemeNum values
instance Arbitrary SchemeNum where
    arbitrary = oneof
        [ SInteger <$> arbitrary
        , do
            n <- arbitrary
            d <- arbitrary `suchThat` (/= 0)
            return $ SRational (n % d)
        , SReal <$> arbitrary
        , do
            r <- arbitrary
            i <- arbitrary
            return $ SComplex (r :+ i)
        ]

-- | Arbitrary instance for generating random LispVal values
instance Arbitrary LispVal where
    arbitrary = sized genLispVal

-- | Generate a LispVal with bounded depth
genLispVal :: Int -> Gen LispVal
genLispVal 0 = genAtomicVal
genLispVal n = frequency
    [ (3, genAtomicVal)
    , (1, genList n)
    , (1, genDottedList n)
    ]

-- | Generate atomic (non-compound) values
genAtomicVal :: Gen LispVal
genAtomicVal = oneof
    [ Atom <$> genAtomName
    , Number <$> arbitrary
    , String <$> genSafeString
    , Char <$> genSafeChar
    , Bool <$> arbitrary
    ]

-- | Generate a valid atom name
genAtomName :: Gen String
genAtomName = do
    first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*+-/<=>?@^_~"
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!$%&*+-/<=>?@^_~"
    return (first : take 10 rest)  -- Limit length

-- | Generate a string without problematic characters
genSafeString :: Gen String
genSafeString = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "

-- | Generate a safe character
genSafeChar :: Gen Char
genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " !@#$%"

-- | Generate a list
genList :: Int -> Gen LispVal
genList n = do
    len <- choose (0, 5)
    elems <- vectorOf len (genLispVal (n `div` 2))
    return $ List elems

-- | Generate a dotted list
genDottedList :: Int -> Gen LispVal
genDottedList n = do
    len <- choose (1, 3)
    elems <- vectorOf len (genLispVal (n `div` 2))
    tail' <- genLispVal (n `div` 2)
    return $ DottedList elems tail'

-- | Arbitrary Scheme expressions as strings
newtype SchemeExpr = SchemeExpr String deriving (Show)

instance Arbitrary SchemeExpr where
    arbitrary = SchemeExpr <$> genExprString

-- | Generate a random Scheme expression string
genExprString :: Gen String
genExprString = sized genExpr'
  where
    genExpr' 0 = genAtomicExpr
    genExpr' n = frequency
        [ (3, genAtomicExpr)
        , (1, genListExpr n)
        , (1, genQuotedExpr n)
        ]

    genAtomicExpr = oneof
        [ genAtomName
        , show <$> (arbitrary :: Gen Integer)
        , do
            n <- arbitrary :: Gen Integer
            d <- (arbitrary :: Gen Integer) `suchThat` (\x -> x /= 0 && abs x < 1000)
            return $ show n ++ "/" ++ show d
        , show <$> (arbitrary :: Gen Double)
        , do
            s <- genSafeString
            return $ "\"" ++ s ++ "\""
        , elements ["#t", "#f"]
        ]

    genListExpr n = do
        len <- choose (0, 4)
        elems <- vectorOf len (genExpr' (n `div` 2))
        return $ "(" ++ unwords elems ++ ")"

    genQuotedExpr n = do
        q <- elements ["'", "`"]
        e <- genExpr' (n `div` 2)
        return $ q ++ e

-- | Arbitrary arithmetic expressions
newtype ArithExpr = ArithExpr String deriving (Show)

instance Arbitrary ArithExpr where
    arbitrary = ArithExpr <$> genArithExpr

genArithExpr :: Gen String
genArithExpr = sized genArith
  where
    genArith 0 = show <$> (arbitrary :: Gen Integer)
    genArith n = frequency
        [ (3, show <$> (arbitrary :: Gen Integer))
        , (1, do
            op <- elements ["+", "-", "*"]
            a <- genArith (n `div` 2)
            b <- genArith (n `div` 2)
            return $ "(" ++ op ++ " " ++ a ++ " " ++ b ++ ")")
        ]

-- | Arbitrary list operations
newtype ListExpr = ListExpr String deriving (Show)

instance Arbitrary ListExpr where
    arbitrary = ListExpr <$> genListOpExpr

genListOpExpr :: Gen String
genListOpExpr = do
    len <- choose (0, 10)
    nums <- vectorOf len (arbitrary :: Gen Integer)
    let listStr = "'(" ++ unwords (map show nums) ++ ")"
    op <- elements
        [ "(length " ++ listStr ++ ")"
        , "(reverse " ++ listStr ++ ")"
        , "(car " ++ listStr ++ ")"
        , "(cdr " ++ listStr ++ ")"
        , "(null? " ++ listStr ++ ")"
        , "(list? " ++ listStr ++ ")"
        ]
    return op


spec :: Spec
spec = describe "Fuzzing" $ do
    describe "Parser fuzzing" $ do
        it "parser does not crash on arbitrary strings" $ property $
            \s -> case readExpr s of
                Left _  -> True  -- Errors are fine
                Right _ -> True  -- Success is fine

        it "parser handles arbitrary valid expressions" $ property $
            \(SchemeExpr s) -> case readExpr s of
                Left _  -> True
                Right _ -> True

        it "parser handles deeply nested expressions" $ property $
            \(Positive n) ->
                let depth = min n 20  -- Limit depth
                    expr = replicate depth '(' ++ "42" ++ replicate depth ')'
                in case readExpr expr of
                    Left _  -> True
                    Right _ -> True

    describe "Evaluator fuzzing" $ do
        it "evaluator does not crash on arithmetic expressions" $ property $
            \(ArithExpr s) -> ioProperty $ do
                env <- primitiveBindings
                result <- runExceptT $ liftThrows (readExpr s) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

        it "evaluator handles list operations" $ property $
            \(ListExpr s) -> ioProperty $ do
                env <- primitiveBindings
                result <- runExceptT $ liftThrows (readExpr s) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

    describe "Numeric tower fuzzing" $ do
        it "addition does not crash" $ property $
            \(a :: Integer) (b :: Integer) -> ioProperty $ do
                env <- primitiveBindings
                let expr = "(+ " ++ show a ++ " " ++ show b ++ ")"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

        it "subtraction does not crash" $ property $
            \(a :: Integer) (b :: Integer) -> ioProperty $ do
                env <- primitiveBindings
                let expr = "(- " ++ show a ++ " " ++ show b ++ ")"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

        it "multiplication does not crash" $ property $
            \(a :: Integer) (b :: Integer) -> ioProperty $ do
                env <- primitiveBindings
                let expr = "(* " ++ show a ++ " " ++ show b ++ ")"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

        it "division handles zero gracefully" $ property $
            \(a :: Integer) -> ioProperty $ do
                env <- primitiveBindings
                let expr = "(/ " ++ show a ++ " 0)"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True   -- Should error on division by zero
                    Right _ -> False  -- Should not succeed

        it "exponentiation does not crash" $ property $
            \(a :: Integer) (Positive (Small b)) -> ioProperty $ do
                env <- primitiveBindings
                let expr = "(expt " ++ show (a `mod` 100) ++ " " ++ show (b `mod` 10) ++ ")"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

    describe "String fuzzing" $ do
        it "string operations do not crash" $ property $
            \s -> ioProperty $ do
                env <- primitiveBindings
                let safeS = filter (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")) s
                    expr = "(string-length \"" ++ take 100 safeS ++ "\")"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

    describe "Type predicates fuzzing" $ do
        it "type predicates do not crash" $ property $
            \val -> ioProperty $ do
                env <- primitiveBindings
                let valStr = showForTest val
                    expr = "(list (number? " ++ valStr ++ ") " ++
                           "(string? " ++ valStr ++ ") " ++
                           "(list? " ++ valStr ++ ") " ++
                           "(symbol? " ++ valStr ++ "))"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

    describe "Comparison fuzzing" $ do
        it "numeric comparisons do not crash" $ property $
            \(a :: Integer) (b :: Integer) -> ioProperty $ do
                env <- primitiveBindings
                let expr = "(list (= " ++ show a ++ " " ++ show b ++ ") " ++
                           "(< " ++ show a ++ " " ++ show b ++ ") " ++
                           "(> " ++ show a ++ " " ++ show b ++ ") " ++
                           "(<= " ++ show a ++ " " ++ show b ++ ") " ++
                           "(>= " ++ show a ++ " " ++ show b ++ "))"
                result <- runExceptT $ liftThrows (readExpr expr) >>= eval env
                return $ case result of
                    Left _  -> True
                    Right _ -> True

-- | Helper to convert LispVal to string for embedding in expressions
showForTest :: LispVal -> String
showForTest (Atom s) = "'" ++ s
showForTest (Number (SInteger n)) = show n
showForTest (Number (SRational r)) = show (Data.Ratio.numerator r) ++ "/" ++ show (Data.Ratio.denominator r)
showForTest (Number (SReal d)) = show d
showForTest (Number (SComplex c)) = show (Data.Complex.realPart c) ++ "+" ++ show (Data.Complex.imagPart c) ++ "i"
showForTest (String s) = "\"" ++ filter (`elem` ['a'..'z']) s ++ "\""
showForTest (Char c) = "#\\" ++ [c]
showForTest (Bool True) = "#t"
showForTest (Bool False) = "#f"
showForTest (List []) = "'()"
showForTest (List xs) = "'(" ++ unwords (map showForTest xs) ++ ")"
showForTest _ = "'()"  -- Default for unsupported types
