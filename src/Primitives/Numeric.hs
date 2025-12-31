{-# LANGUAGE FlexibleContexts #-}

-- | Numeric primitives for R5RS Scheme
--
-- This module implements the full Scheme numeric tower:
--
-- * Integer operations (exact)
-- * Rational operations (exact)
-- * Real operations (inexact)
-- * Complex operations
--
-- It also includes arithmetic, comparison, rounding, transcendental
-- functions, and exactness conversions.
module Primitives.Numeric
    ( -- * Primitive bindings
      numericPrimitives
      -- * Arithmetic operations
    , addNum
    , subNum
    , mulNum
    , divNum
    , negateNum
    , absNum
    , numExpt
      -- * Fold operations
    , numericFold
    , numericFold2
    , numericMinus
    , numericDiv
    , integerBinop
    , integerFold
      -- * Type predicates
    , isNumber
    , isComplex
    , isReal
    , isRational
    , isInteger
    , isExactP
    , isInexactP
      -- * Numeric predicates
    , isZero
    , isPositive
    , isNegative
    , isOdd
    , isEven
      -- * Comparison
    , numCompare
    , realCompare
      -- * Rounding
    , floorNum
    , ceilingNum
    , truncateNum
    , roundNum
      -- * Rational operations
    , numeratorNum
    , denominatorNum
    , rationalizeNum
      -- * Transcendental functions
    , unaryTranscendental
    , logNum
    , atanNum
    , sqrtNum
      -- * Complex number operations
    , makeRectangular
    , makePolar'
    , realPartNum
    , imagPartNum
    , magnitudeNum
    , angleNum
      -- * Exactness conversions
    , exactToInexact
    , inexactToExact
      -- * Helper functions
    , unaryNum
    , unpackNum
    , promoteAndApply
    , maxNum
    , minNum
    ) where

import Types
import Control.Monad (foldM)
import Control.Monad.Except
import Data.Ratio (numerator, denominator, (%))
import Data.Complex (Complex(..), mkPolar, realPart, imagPart, magnitude, phase)

-- | All numeric primitive functions
numericPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numericPrimitives =
    -- Arithmetic with full numeric tower
    [ ("+", numericFold addNum (SInteger 0))
    , ("-", numericMinus)
    , ("*", numericFold mulNum (SInteger 1))
    , ("/", numericDiv)
    , ("quotient", integerBinop quot)
    , ("remainder", integerBinop rem)
    , ("modulo", integerBinop mod)
    , ("abs", unaryNum absNum)
    , ("max", numericFold2 maxNum)
    , ("min", numericFold2 minNum)
    , ("gcd", integerFold gcd 0)
    , ("lcm", integerFold lcm 1)
    , ("expt", numExpt)

    -- Numeric type predicates
    , ("number?", unaryOp isNumber)
    , ("complex?", unaryOp isComplex)
    , ("real?", unaryOp isReal)
    , ("rational?", unaryOp isRational)
    , ("integer?", unaryOp isInteger)
    , ("exact?", unaryOp isExactP)
    , ("inexact?", unaryOp isInexactP)

    -- Numeric predicates
    , ("zero?", unaryNum isZero)
    , ("positive?", unaryNum isPositive)
    , ("negative?", unaryNum isNegative)
    , ("odd?", unaryNum isOdd)
    , ("even?", unaryNum isEven)

    -- Numeric comparison
    , ("=", numCompare (==))
    , ("<", realCompare (<))
    , (">", realCompare (>))
    , ("<=", realCompare (<=))
    , (">=", realCompare (>=))

    -- Rounding
    , ("floor", unaryNum floorNum)
    , ("ceiling", unaryNum ceilingNum)
    , ("truncate", unaryNum truncateNum)
    , ("round", unaryNum roundNum)

    -- Rational operations
    , ("numerator", unaryNum numeratorNum)
    , ("denominator", unaryNum denominatorNum)
    , ("rationalize", rationalizeNum)

    -- Transcendental functions
    , ("exp", unaryTranscendental exp)
    , ("log", logNum)
    , ("sin", unaryTranscendental sin)
    , ("cos", unaryTranscendental cos)
    , ("tan", unaryTranscendental tan)
    , ("asin", unaryTranscendental asin)
    , ("acos", unaryTranscendental acos)
    , ("atan", atanNum)
    , ("sqrt", sqrtNum)

    -- Complex number operations
    , ("make-rectangular", makeRectangular)
    , ("make-polar", makePolar')
    , ("real-part", unaryNum realPartNum)
    , ("imag-part", unaryNum imagPartNum)
    , ("magnitude", unaryNum magnitudeNum)
    , ("angle", unaryNum angleNum)

    -- Exactness conversions
    , ("exact->inexact", unaryNum exactToInexact)
    , ("inexact->exact", unaryNum inexactToExact)
    ]

-- | Apply a unary function to a single argument
unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ args = throwError $ NumArgs 1 args

-- | Apply a unary numeric function
unaryNum :: (SchemeNum -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryNum f [Number n] = f n
unaryNum _ [v] = throwError $ TypeMismatch "number" v
unaryNum _ args = throwError $ NumArgs 1 args

-- | Extract a SchemeNum from a LispVal
unpackNum :: LispVal -> ThrowsError SchemeNum
unpackNum (Number n) = return n
unpackNum v = throwError $ TypeMismatch "number" v

-- | Promote two numbers to a common type and apply operation
promoteAndApply :: (SchemeNum -> SchemeNum -> SchemeNum) -> SchemeNum -> SchemeNum -> SchemeNum
promoteAndApply op a b = simplifyNum $ case (a, b) of
    (SInteger _, SInteger _)   -> op a b
    (SInteger n, SRational _)  -> op (SRational (n % 1)) b
    (SRational _, SInteger n)  -> op a (SRational (n % 1))
    (SRational _, SRational _) -> op a b
    (SComplex _, _)            -> op a (promoteToSchemeComplex b)
    (_, SComplex _)            -> op (promoteToSchemeComplex a) b
    _                          -> op (toSchemeReal a) (toSchemeReal b)
  where
    toSchemeReal (SInteger n)  = SReal (fromIntegral n)
    toSchemeReal (SRational r) = SReal (fromRational r)
    toSchemeReal x             = x
    promoteToSchemeComplex (SInteger n)  = SComplex (fromIntegral n :+ 0)
    promoteToSchemeComplex (SRational r) = SComplex (fromRational r :+ 0)
    promoteToSchemeComplex (SReal d)     = SComplex (d :+ 0)
    promoteToSchemeComplex x             = x

-- | Addition with type promotion
addNum :: SchemeNum -> SchemeNum -> SchemeNum
addNum (SInteger a) (SInteger b) = SInteger (a + b)
addNum (SRational a) (SRational b) = SRational (a + b)
addNum (SReal a) (SReal b) = SReal (a + b)
addNum (SComplex a) (SComplex b) = SComplex (a + b)
addNum a b = promoteAndApply addNum a b

-- | Multiplication with type promotion
mulNum :: SchemeNum -> SchemeNum -> SchemeNum
mulNum (SInteger a) (SInteger b) = SInteger (a * b)
mulNum (SRational a) (SRational b) = SRational (a * b)
mulNum (SReal a) (SReal b) = SReal (a * b)
mulNum (SComplex a) (SComplex b) = SComplex (a * b)
mulNum a b = promoteAndApply mulNum a b

-- | Subtraction with type promotion
subNum :: SchemeNum -> SchemeNum -> SchemeNum
subNum (SInteger a) (SInteger b) = SInteger (a - b)
subNum (SRational a) (SRational b) = SRational (a - b)
subNum (SReal a) (SReal b) = SReal (a - b)
subNum (SComplex a) (SComplex b) = SComplex (a - b)
subNum a b = promoteAndApply subNum a b

-- | Division with type promotion and error checking
divNum :: SchemeNum -> SchemeNum -> ThrowsError SchemeNum
divNum _ (SInteger 0) = throwError DivideByZero
divNum _ (SRational r) | r == 0 = throwError DivideByZero
divNum _ (SReal 0) = throwError DivideByZero
divNum (SInteger a) (SInteger b)
    | a `mod` b == 0 = return $ SInteger (a `div` b)
    | otherwise = return $ simplifyNum $ SRational (a % b)
divNum (SRational a) (SRational b) = return $ simplifyNum $ SRational (a / b)
divNum (SReal a) (SReal b) = return $ SReal (a / b)
divNum (SComplex a) (SComplex b) = return $ simplifyNum $ SComplex (a / b)
divNum a b = divNum (promote a b) (promote b a)
  where
    promote x@(SComplex _) _ = x
    promote _ (SComplex _) = promoteToSchemeComplex x
      where x = a
    promote x@(SReal _) _ = x
    promote _ (SReal _) = toSchemeReal x
      where x = a
    promote x@(SRational _) _ = x
    promote _ (SRational _) = toSchemeRational x
      where x = a
    promote x _ = x
    toSchemeReal (SInteger n) = SReal (fromIntegral n)
    toSchemeReal (SRational r) = SReal (fromRational r)
    toSchemeReal x = x
    toSchemeRational (SInteger n) = SRational (n % 1)
    toSchemeRational x = x
    promoteToSchemeComplex (SInteger n) = SComplex (fromIntegral n :+ 0)
    promoteToSchemeComplex (SRational r) = SComplex (fromRational r :+ 0)
    promoteToSchemeComplex (SReal d) = SComplex (d :+ 0)
    promoteToSchemeComplex x = x

-- | Maximum of two numbers
maxNum :: SchemeNum -> SchemeNum -> SchemeNum
maxNum a b = if schemeNumToDouble a >= schemeNumToDouble b then a else b

-- | Minimum of two numbers
minNum :: SchemeNum -> SchemeNum -> SchemeNum
minNum a b = if schemeNumToDouble a <= schemeNumToDouble b then a else b

-- | Negate a number
negateNum :: SchemeNum -> SchemeNum
negateNum (SInteger n) = SInteger (-n)
negateNum (SRational r) = SRational (-r)
negateNum (SReal d) = SReal (-d)
negateNum (SComplex c) = SComplex (-c)

-- | Absolute value
absNum :: SchemeNum -> ThrowsError LispVal
absNum (SInteger n) = return $ Number $ SInteger (abs n)
absNum (SRational r) = return $ Number $ SRational (abs r)
absNum (SReal d) = return $ Number $ SReal (abs d)
absNum (SComplex c) = return $ Number $ SReal (magnitude c)

-- | Fold a binary operation over a list of numbers
numericFold :: (SchemeNum -> SchemeNum -> SchemeNum) -> SchemeNum -> [LispVal] -> ThrowsError LispVal
numericFold _ identity [] = return $ Number identity
numericFold op _ args = do
    nums <- mapM unpackNum args
    return $ Number $ foldl1 (\a b -> simplifyNum $ op a b) nums

-- | Fold requiring at least one argument
numericFold2 :: (SchemeNum -> SchemeNum -> SchemeNum) -> [LispVal] -> ThrowsError LispVal
numericFold2 _ [] = throwError $ NumArgs 1 []
numericFold2 _ [n] = return n
numericFold2 op args = do
    nums <- mapM unpackNum args
    return $ Number $ foldl1 op nums

-- | Subtraction: unary negation or binary/n-ary subtraction
numericMinus :: [LispVal] -> ThrowsError LispVal
numericMinus [] = throwError $ NumArgs 1 []
numericMinus [n] = do
    num <- unpackNum n
    return $ Number $ negateNum num
numericMinus args = do
    nums <- mapM unpackNum args
    return $ Number $ foldl1 subNum nums

-- | Division: unary reciprocal or binary/n-ary division
numericDiv :: [LispVal] -> ThrowsError LispVal
numericDiv [] = throwError $ NumArgs 1 []
numericDiv [n] = do
    num <- unpackNum n
    result <- divNum (SInteger 1) num
    return $ Number result
numericDiv args = do
    nums <- mapM unpackNum args
    result <- foldM (\a b -> divNum a b) (head nums) (tail nums)
    return $ Number $ simplifyNum result

-- | Binary integer operation
integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop _ [Number (SInteger _)] = throwError $ NumArgs 2 []
integerBinop op [Number (SInteger a), Number (SInteger b)] = return $ Number $ SInteger $ op a b
integerBinop op [Number a, Number b] = return $ Number $ SInteger $ op (round $ schemeNumToDouble a) (round $ schemeNumToDouble b)
integerBinop _ args = throwError $ NumArgs 2 args

-- | Fold an integer operation
integerFold :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> ThrowsError LispVal
integerFold _ identity [] = return $ Number $ SInteger identity
integerFold op _ args = do
    nums <- mapM unpackNum args
    let ints = map (round . schemeNumToDouble) nums
    return $ Number $ SInteger $ foldl1 op ints

-- | Exponentiation
numExpt :: [LispVal] -> ThrowsError LispVal
numExpt [Number base, Number (SInteger exp')] = return $ Number $ simplifyNum $ exptNum base exp'
  where
    exptNum (SInteger b) e
        | e >= 0    = SInteger (b ^ e)
        | otherwise = SRational (1 % (b ^ (-e)))
    exptNum (SRational b) e
        | e >= 0    = SRational (b ^^ e)
        | otherwise = SRational (1 / (b ^^ (-e)))
    exptNum (SReal b) e = SReal (b ** fromIntegral e)
    exptNum (SComplex b) e = SComplex (b ** fromIntegral e)
numExpt [Number base, Number exp'] = return $ Number $ SReal $ schemeNumToDouble base ** schemeNumToDouble exp'
numExpt args = throwError $ NumArgs 2 args

-- | Check if value is a number
isNumber :: LispVal -> ThrowsError LispVal
isNumber (Number _) = return $ Bool True
isNumber _ = return $ Bool False

-- | Check if value is complex (all numbers are complex in Scheme)
isComplex :: LispVal -> ThrowsError LispVal
isComplex (Number _) = return $ Bool True
isComplex _ = return $ Bool False

-- | Check if value is real
isReal :: LispVal -> ThrowsError LispVal
isReal (Number (SComplex c)) = return $ Bool $ imagPart c == 0
isReal (Number _) = return $ Bool True
isReal _ = return $ Bool False

-- | Check if value is rational
isRational :: LispVal -> ThrowsError LispVal
isRational (Number (SInteger _)) = return $ Bool True
isRational (Number (SRational _)) = return $ Bool True
isRational (Number (SReal d)) = return $ Bool $ not (isNaN d || isInfinite d)
isRational (Number (SComplex c)) = return $ Bool $ imagPart c == 0 && not (isNaN (realPart c) || isInfinite (realPart c))
isRational _ = return $ Bool False

-- | Check if value is an integer
isInteger :: LispVal -> ThrowsError LispVal
isInteger (Number (SInteger _)) = return $ Bool True
isInteger (Number (SRational r)) = return $ Bool $ denominator r == 1
isInteger (Number (SReal d)) = return $ Bool $ d == fromIntegral (round d :: Integer)
isInteger (Number (SComplex c)) = return $ Bool $ imagPart c == 0 && realPart c == fromIntegral (round (realPart c) :: Integer)
isInteger _ = return $ Bool False

-- | Check if number is exact
isExactP :: LispVal -> ThrowsError LispVal
isExactP (Number n) = return $ Bool $ isExact n
isExactP v = throwError $ TypeMismatch "number" v

-- | Check if number is inexact
isInexactP :: LispVal -> ThrowsError LispVal
isInexactP (Number n) = return $ Bool $ not $ isExact n
isInexactP v = throwError $ TypeMismatch "number" v

-- | Check if number is zero
isZero :: SchemeNum -> ThrowsError LispVal
isZero (SInteger 0) = return $ Bool True
isZero (SRational r) = return $ Bool $ r == 0
isZero (SReal 0) = return $ Bool True
isZero (SComplex c) = return $ Bool $ c == 0
isZero _ = return $ Bool False

-- | Check if number is positive
isPositive :: SchemeNum -> ThrowsError LispVal
isPositive (SInteger n) = return $ Bool $ n > 0
isPositive (SRational r) = return $ Bool $ r > 0
isPositive (SReal d) = return $ Bool $ d > 0
isPositive (SComplex _) = throwError $ Default "positive? not defined for complex"

-- | Check if number is negative
isNegative :: SchemeNum -> ThrowsError LispVal
isNegative (SInteger n) = return $ Bool $ n < 0
isNegative (SRational r) = return $ Bool $ r < 0
isNegative (SReal d) = return $ Bool $ d < 0
isNegative (SComplex _) = throwError $ Default "negative? not defined for complex"

-- | Check if number is odd
isOdd :: SchemeNum -> ThrowsError LispVal
isOdd (SInteger n) = return $ Bool $ odd n
isOdd n = return $ Bool $ odd (round $ schemeNumToDouble n :: Integer)

-- | Check if number is even
isEven :: SchemeNum -> ThrowsError LispVal
isEven (SInteger n) = return $ Bool $ even n
isEven n = return $ Bool $ even (round $ schemeNumToDouble n :: Integer)

-- | Numeric equality comparison
numCompare :: (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
numCompare _ [] = throwError $ NumArgs 2 []
numCompare _ [_] = throwError $ NumArgs 2 []
numCompare op args = do
    nums <- mapM unpackNum args
    let doubles = map schemeNumToDouble nums
    return $ Bool $ all (uncurry op) $ zip doubles (tail doubles)

-- | Real number comparison (rejects complex with non-zero imaginary)
realCompare :: (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
realCompare _ [] = throwError $ NumArgs 2 []
realCompare _ [_] = throwError $ NumArgs 2 []
realCompare op args = do
    nums <- mapM unpackNum args
    mapM_ checkReal nums
    let doubles = map schemeNumToDouble nums
    return $ Bool $ all (uncurry op) $ zip doubles (tail doubles)
  where
    checkReal (SComplex c) | imagPart c /= 0 = throwError $ Default "real number required"
    checkReal _ = return ()

-- | Floor function
floorNum :: SchemeNum -> ThrowsError LispVal
floorNum (SInteger n) = return $ Number $ SInteger n
floorNum (SRational r) = return $ Number $ SInteger $ floor r
floorNum (SReal d) = return $ Number $ SInteger $ floor d
floorNum (SComplex _) = throwError $ Default "floor not defined for complex"

-- | Ceiling function
ceilingNum :: SchemeNum -> ThrowsError LispVal
ceilingNum (SInteger n) = return $ Number $ SInteger n
ceilingNum (SRational r) = return $ Number $ SInteger $ ceiling r
ceilingNum (SReal d) = return $ Number $ SInteger $ ceiling d
ceilingNum (SComplex _) = throwError $ Default "ceiling not defined for complex"

-- | Truncate function
truncateNum :: SchemeNum -> ThrowsError LispVal
truncateNum (SInteger n) = return $ Number $ SInteger n
truncateNum (SRational r) = return $ Number $ SInteger $ truncate r
truncateNum (SReal d) = return $ Number $ SInteger $ truncate d
truncateNum (SComplex _) = throwError $ Default "truncate not defined for complex"

-- | Round function
roundNum :: SchemeNum -> ThrowsError LispVal
roundNum (SInteger n) = return $ Number $ SInteger n
roundNum (SRational r) = return $ Number $ SInteger $ round r
roundNum (SReal d) = return $ Number $ SInteger $ round d
roundNum (SComplex _) = throwError $ Default "round not defined for complex"

-- | Get numerator of a number
numeratorNum :: SchemeNum -> ThrowsError LispVal
numeratorNum (SInteger n) = return $ Number $ SInteger n
numeratorNum (SRational r) = return $ Number $ SInteger $ numerator r
numeratorNum (SReal d) = return $ Number $ SInteger $ numerator $ toRational d
numeratorNum (SComplex _) = throwError $ Default "numerator not defined for complex"

-- | Get denominator of a number
denominatorNum :: SchemeNum -> ThrowsError LispVal
denominatorNum (SInteger _) = return $ Number $ SInteger 1
denominatorNum (SRational r) = return $ Number $ SInteger $ denominator r
denominatorNum (SReal d) = return $ Number $ SInteger $ denominator $ toRational d
denominatorNum (SComplex _) = throwError $ Default "denominator not defined for complex"

-- | Rationalize a number within an epsilon
rationalizeNum :: [LispVal] -> ThrowsError LispVal
rationalizeNum [Number x, Number y] = do
    let xd = schemeNumToDouble x
    let yd = abs $ schemeNumToDouble y
    return $ Number $ simplifyNum $ SRational $ approxRational xd yd
  where
    approxRational x' eps = findRational x' eps 1 1
    findRational x' eps n d
        | abs (x' - fromIntegral n / fromIntegral d) <= eps = n % d
        | n % d < toRational x' = findRational x' eps (n + 1) d
        | otherwise = findRational x' eps n (d + 1)
rationalizeNum args = throwError $ NumArgs 2 args

-- | Apply a transcendental function
unaryTranscendental :: (Double -> Double) -> [LispVal] -> ThrowsError LispVal
unaryTranscendental f [Number n] = return $ Number $ SReal $ f $ schemeNumToDouble n
unaryTranscendental _ args = throwError $ NumArgs 1 args

-- | Logarithm (natural or with base)
logNum :: [LispVal] -> ThrowsError LispVal
logNum [Number n] = return $ Number $ SReal $ log $ schemeNumToDouble n
logNum [Number n, Number base] = return $ Number $ SReal $ logBase (schemeNumToDouble base) (schemeNumToDouble n)
logNum args = throwError $ NumArgs 1 args

-- | Arctangent (one or two argument form)
atanNum :: [LispVal] -> ThrowsError LispVal
atanNum [Number n] = return $ Number $ SReal $ atan $ schemeNumToDouble n
atanNum [Number y, Number x] = return $ Number $ SReal $ atan2 (schemeNumToDouble y) (schemeNumToDouble x)
atanNum args = throwError $ NumArgs 1 args

-- | Square root (returns complex for negative inputs)
sqrtNum :: [LispVal] -> ThrowsError LispVal
sqrtNum [Number n] =
    let d = schemeNumToDouble n
    in if d < 0
        then return $ Number $ SComplex (0 :+ sqrt (-d))
        else return $ Number $ simplifyNum $ SReal $ sqrt d
sqrtNum args = throwError $ NumArgs 1 args

-- | Create complex from rectangular coordinates
makeRectangular :: [LispVal] -> ThrowsError LispVal
makeRectangular [Number r, Number i] =
    return $ Number $ simplifyNum $ SComplex (schemeNumToDouble r :+ schemeNumToDouble i)
makeRectangular args = throwError $ NumArgs 2 args

-- | Create complex from polar coordinates
makePolar' :: [LispVal] -> ThrowsError LispVal
makePolar' [Number mag, Number ang] =
    return $ Number $ simplifyNum $ SComplex $ mkPolar (schemeNumToDouble mag) (schemeNumToDouble ang)
makePolar' args = throwError $ NumArgs 2 args

-- | Get real part of a number
realPartNum :: SchemeNum -> ThrowsError LispVal
realPartNum (SComplex c) = return $ Number $ simplifyNum $ SReal $ realPart c
realPartNum n = return $ Number n

-- | Get imaginary part of a number
imagPartNum :: SchemeNum -> ThrowsError LispVal
imagPartNum (SComplex c) = return $ Number $ simplifyNum $ SReal $ imagPart c
imagPartNum _ = return $ Number $ SInteger 0

-- | Get magnitude of a number
magnitudeNum :: SchemeNum -> ThrowsError LispVal
magnitudeNum (SComplex c) = return $ Number $ SReal $ magnitude c
magnitudeNum n = return $ Number $ SReal $ abs $ schemeNumToDouble n

-- | Get angle/phase of a number
angleNum :: SchemeNum -> ThrowsError LispVal
angleNum (SComplex c) = return $ Number $ SReal $ phase c
angleNum n
    | schemeNumToDouble n >= 0 = return $ Number $ SReal 0
    | otherwise = return $ Number $ SReal pi

-- | Convert exact to inexact
exactToInexact :: SchemeNum -> ThrowsError LispVal
exactToInexact (SInteger n) = return $ Number $ SReal $ fromIntegral n
exactToInexact (SRational r) = return $ Number $ SReal $ fromRational r
exactToInexact n = return $ Number n

-- | Convert inexact to exact
inexactToExact :: SchemeNum -> ThrowsError LispVal
inexactToExact (SReal d)
    | d == fromIntegral (round d :: Integer) = return $ Number $ SInteger $ round d
    | otherwise = return $ Number $ SRational $ toRational d
inexactToExact (SComplex c)
    | imagPart c == 0 = inexactToExact (SReal $ realPart c)
    | otherwise = throwError $ Default "cannot convert complex to exact"
inexactToExact n = return $ Number n
