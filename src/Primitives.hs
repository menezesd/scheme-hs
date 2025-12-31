{-# LANGUAGE FlexibleContexts #-}

module Primitives
    ( primitiveBindings
    , primitives
    , ioPrimitives
    , eqv
    ) where

import Types
import Env
import Parser (readExpr, readExprList)
import Control.Monad (zipWithM, foldM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Array (listArray, bounds, elems, (!), (//))
import Data.Ratio (numerator, denominator, (%))
import Data.Complex (Complex(..), mkPolar, realPart, imagPart, magnitude, phase)
import Data.Char (toUpper, toLower, isAlpha, isDigit, isSpace, isUpper, isLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO
import Control.Exception (try, IOException)

-- | Create environment with all primitives bound
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives
                                            ++ map makeIOFunc ioPrimitives)
  where
    makePrimitiveFunc (name, func) = (name, PrimitiveFunc func)
    makeIOFunc (name, func) = (name, IOFunc func)

-- | All primitive functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
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

    -- Boolean operations
    , ("not", unaryOp notOp)
    , ("boolean?", unaryOp isBool)

    -- Type predicates
    , ("symbol?", unaryOp isSymbol)
    , ("string?", unaryOp isString)
    , ("char?", unaryOp isChar)
    , ("pair?", unaryOp isPair)
    , ("list?", unaryOp isList)
    , ("null?", unaryOp isNull)
    , ("vector?", unaryOp isVector)
    , ("procedure?", unaryOp isProcedure)

    -- Symbol operations
    , ("symbol->string", unaryOp symbolToString)
    , ("string->symbol", unaryOp stringToSymbol)

    -- String operations
    , ("string-length", unaryOp stringLength)
    , ("string-ref", stringRef)
    , ("string-append", stringAppend)
    , ("substring", substring)
    , ("string=?", strBoolBinop (==))
    , ("string<?", strBoolBinop (<))
    , ("string>?", strBoolBinop (>))
    , ("string<=?", strBoolBinop (<=))
    , ("string>=?", strBoolBinop (>=))
    , ("string-ci=?", strCiBoolBinop (==))
    , ("string-ci<?", strCiBoolBinop (<))
    , ("string-ci>?", strCiBoolBinop (>))
    , ("string-ci<=?", strCiBoolBinop (<=))
    , ("string-ci>=?", strCiBoolBinop (>=))
    , ("string->list", unaryOp stringToList)
    , ("list->string", unaryOp listToString)
    , ("make-string", makeString)
    , ("string", stringFromChars)
    , ("string-copy", unaryOp stringCopy)
    , ("number->string", numberToString)
    , ("string->number", stringToNumber)

    -- Character operations
    , ("char=?", charBoolBinop (==))
    , ("char<?", charBoolBinop (<))
    , ("char>?", charBoolBinop (>))
    , ("char<=?", charBoolBinop (<=))
    , ("char>=?", charBoolBinop (>=))
    , ("char-ci=?", charCiBoolBinop (==))
    , ("char-ci<?", charCiBoolBinop (<))
    , ("char-ci>?", charCiBoolBinop (>))
    , ("char-ci<=?", charCiBoolBinop (<=))
    , ("char-ci>=?", charCiBoolBinop (>=))
    , ("char->integer", unaryOp charToInteger)
    , ("integer->char", unaryOp integerToChar)
    , ("char-upcase", unaryOp charUpcase)
    , ("char-downcase", unaryOp charDowncase)
    , ("char-alphabetic?", unaryOp charAlphabetic)
    , ("char-numeric?", unaryOp charNumeric)
    , ("char-whitespace?", unaryOp charWhitespace)
    , ("char-upper-case?", unaryOp charUpperCase)
    , ("char-lower-case?", unaryOp charLowerCase)

    -- List operations
    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    , ("list", list')
    , ("length", listLength)
    , ("append", appendLists)
    , ("reverse", reverseList)
    , ("list-ref", listRef)
    , ("list-tail", listTail)
    , ("memq", memq)
    , ("memv", memv)
    , ("member", member)
    , ("assq", assq)
    , ("assv", assv)
    , ("assoc", assoc')

    -- Vector operations
    , ("vector", vector)
    , ("make-vector", makeVector)
    , ("vector-length", vectorLength)
    , ("vector-ref", vectorRef)
    , ("vector-set!", vectorSet)
    , ("vector->list", vectorToList)
    , ("list->vector", listToVector)
    , ("vector-fill!", vectorFill)

    -- String mutation
    , ("string-set!", stringSet)
    , ("string-fill!", stringFill)

    -- Equivalence predicates
    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)

    -- Port predicates
    , ("port?", unaryOp isPort)
    , ("input-port?", unaryOp isInputPort)
    , ("output-port?", unaryOp isOutputPort)
    , ("eof-object?", unaryOp isEOFObject)

    -- Misc
    , ("error", errorProc)

    -- Values (multiple return values)
    , ("values", valuesProc)
    ]

-- | IO primitives
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
    [ ("display", displayProc)
    , ("newline", newlineProc)
    , ("write", writeProc)
    , ("read", readProc)
    , ("load", load)
    -- Port operations
    , ("open-input-file", openInputFile)
    , ("open-output-file", openOutputFile)
    , ("close-input-port", closePort)
    , ("close-output-port", closePort)
    , ("current-input-port", currentInputPort)
    , ("current-output-port", currentOutputPort)
    -- Character I/O
    , ("read-char", readChar)
    , ("peek-char", peekChar)
    , ("write-char", writeChar)
    , ("char-ready?", charReady)
    -- call-with-*-file operations
    , ("call-with-input-file", callWithInputFile)
    , ("call-with-output-file", callWithOutputFile)
    -- with-*-file operations (simplified - don't actually redirect)
    , ("with-input-from-file", withInputFromFile)
    , ("with-output-to-file", withOutputToFile)
    -- Mutable pair operations
    , ("mcons", mcons)           -- Create a mutable pair
    , ("mcar", mcar)             -- Get car of mutable pair
    , ("mcdr", mcdr)             -- Get cdr of mutable pair
    , ("mpair?", mpairQ)         -- Check if mutable pair
    , ("set-car!", setCar)
    , ("set-cdr!", setCdr)
    -- Environment procedures
    , ("scheme-report-environment", schemeReportEnv)
    , ("null-environment", nullEnvProc)
    , ("interaction-environment", interactionEnv)
    -- Multiple values
    , ("call-with-values", callWithValues)
    -- Transcript (no-ops - optional in R5RS)
    , ("transcript-on", transcriptOn)
    , ("transcript-off", transcriptOff)
    ]

-- Helper functions

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ args = throwError $ NumArgs 1 args

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

-- Numeric operations with type promotion

addNum :: SchemeNum -> SchemeNum -> SchemeNum
addNum (SInteger a) (SInteger b) = SInteger (a + b)
addNum (SRational a) (SRational b) = SRational (a + b)
addNum (SReal a) (SReal b) = SReal (a + b)
addNum (SComplex a) (SComplex b) = SComplex (a + b)
addNum a b = promoteAndApply addNum a b

mulNum :: SchemeNum -> SchemeNum -> SchemeNum
mulNum (SInteger a) (SInteger b) = SInteger (a * b)
mulNum (SRational a) (SRational b) = SRational (a * b)
mulNum (SReal a) (SReal b) = SReal (a * b)
mulNum (SComplex a) (SComplex b) = SComplex (a * b)
mulNum a b = promoteAndApply mulNum a b

subNum :: SchemeNum -> SchemeNum -> SchemeNum
subNum (SInteger a) (SInteger b) = SInteger (a - b)
subNum (SRational a) (SRational b) = SRational (a - b)
subNum (SReal a) (SReal b) = SReal (a - b)
subNum (SComplex a) (SComplex b) = SComplex (a - b)
subNum a b = promoteAndApply subNum a b

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

maxNum :: SchemeNum -> SchemeNum -> SchemeNum
maxNum a b = if schemeNumToDouble a >= schemeNumToDouble b then a else b

minNum :: SchemeNum -> SchemeNum -> SchemeNum
minNum a b = if schemeNumToDouble a <= schemeNumToDouble b then a else b

negateNum :: SchemeNum -> SchemeNum
negateNum (SInteger n) = SInteger (-n)
negateNum (SRational r) = SRational (-r)
negateNum (SReal d) = SReal (-d)
negateNum (SComplex c) = SComplex (-c)

absNum :: SchemeNum -> ThrowsError LispVal
absNum (SInteger n) = return $ Number $ SInteger (abs n)
absNum (SRational r) = return $ Number $ SRational (abs r)
absNum (SReal d) = return $ Number $ SReal (abs d)
absNum (SComplex c) = return $ Number $ SReal (magnitude c)

-- Fold operations

numericFold :: (SchemeNum -> SchemeNum -> SchemeNum) -> SchemeNum -> [LispVal] -> ThrowsError LispVal
numericFold _ identity [] = return $ Number identity
numericFold op _ args = do
    nums <- mapM unpackNum args
    return $ Number $ foldl1 (\a b -> simplifyNum $ op a b) nums

numericFold2 :: (SchemeNum -> SchemeNum -> SchemeNum) -> [LispVal] -> ThrowsError LispVal
numericFold2 _ [] = throwError $ NumArgs 1 []
numericFold2 _ [n] = return n
numericFold2 op args = do
    nums <- mapM unpackNum args
    return $ Number $ foldl1 op nums

numericMinus :: [LispVal] -> ThrowsError LispVal
numericMinus [] = throwError $ NumArgs 1 []
numericMinus [n] = do
    num <- unpackNum n
    return $ Number $ negateNum num
numericMinus args = do
    nums <- mapM unpackNum args
    return $ Number $ foldl1 subNum nums

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

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop _ [Number (SInteger _)] = throwError $ NumArgs 2 []
integerBinop op [Number (SInteger a), Number (SInteger b)] = return $ Number $ SInteger $ op a b
integerBinop op [Number a, Number b] = return $ Number $ SInteger $ op (round $ schemeNumToDouble a) (round $ schemeNumToDouble b)
integerBinop _ args = throwError $ NumArgs 2 args

integerFold :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> ThrowsError LispVal
integerFold _ identity [] = return $ Number $ SInteger identity
integerFold op _ args = do
    nums <- mapM unpackNum args
    let ints = map (round . schemeNumToDouble) nums
    return $ Number $ SInteger $ foldl1 op ints

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

-- Numeric type predicates

isNumber :: LispVal -> ThrowsError LispVal
isNumber (Number _) = return $ Bool True
isNumber _ = return $ Bool False

isComplex :: LispVal -> ThrowsError LispVal
isComplex (Number _) = return $ Bool True  -- All numbers are complex
isComplex _ = return $ Bool False

isReal :: LispVal -> ThrowsError LispVal
isReal (Number (SComplex c)) = return $ Bool $ imagPart c == 0
isReal (Number _) = return $ Bool True
isReal _ = return $ Bool False

isRational :: LispVal -> ThrowsError LispVal
isRational (Number (SInteger _)) = return $ Bool True
isRational (Number (SRational _)) = return $ Bool True
isRational (Number (SReal d)) = return $ Bool $ not (isNaN d || isInfinite d)
isRational (Number (SComplex c)) = return $ Bool $ imagPart c == 0 && not (isNaN (realPart c) || isInfinite (realPart c))
isRational _ = return $ Bool False

isInteger :: LispVal -> ThrowsError LispVal
isInteger (Number (SInteger _)) = return $ Bool True
isInteger (Number (SRational r)) = return $ Bool $ denominator r == 1
isInteger (Number (SReal d)) = return $ Bool $ d == fromIntegral (round d :: Integer)
isInteger (Number (SComplex c)) = return $ Bool $ imagPart c == 0 && realPart c == fromIntegral (round (realPart c) :: Integer)
isInteger _ = return $ Bool False

isExactP :: LispVal -> ThrowsError LispVal
isExactP (Number n) = return $ Bool $ isExact n
isExactP v = throwError $ TypeMismatch "number" v

isInexactP :: LispVal -> ThrowsError LispVal
isInexactP (Number n) = return $ Bool $ not $ isExact n
isInexactP v = throwError $ TypeMismatch "number" v

-- Numeric predicates

isZero :: SchemeNum -> ThrowsError LispVal
isZero (SInteger 0) = return $ Bool True
isZero (SRational r) = return $ Bool $ r == 0
isZero (SReal 0) = return $ Bool True
isZero (SComplex c) = return $ Bool $ c == 0
isZero _ = return $ Bool False

isPositive :: SchemeNum -> ThrowsError LispVal
isPositive (SInteger n) = return $ Bool $ n > 0
isPositive (SRational r) = return $ Bool $ r > 0
isPositive (SReal d) = return $ Bool $ d > 0
isPositive (SComplex _) = throwError $ Default "positive? not defined for complex"

isNegative :: SchemeNum -> ThrowsError LispVal
isNegative (SInteger n) = return $ Bool $ n < 0
isNegative (SRational r) = return $ Bool $ r < 0
isNegative (SReal d) = return $ Bool $ d < 0
isNegative (SComplex _) = throwError $ Default "negative? not defined for complex"

isOdd :: SchemeNum -> ThrowsError LispVal
isOdd (SInteger n) = return $ Bool $ odd n
isOdd n = return $ Bool $ odd (round $ schemeNumToDouble n :: Integer)

isEven :: SchemeNum -> ThrowsError LispVal
isEven (SInteger n) = return $ Bool $ even n
isEven n = return $ Bool $ even (round $ schemeNumToDouble n :: Integer)

-- Comparison

numCompare :: (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
numCompare _ [] = throwError $ NumArgs 2 []
numCompare _ [_] = throwError $ NumArgs 2 []
numCompare op args = do
    nums <- mapM unpackNum args
    let doubles = map schemeNumToDouble nums
    return $ Bool $ all (uncurry op) $ zip doubles (tail doubles)

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

-- Rounding functions

floorNum :: SchemeNum -> ThrowsError LispVal
floorNum (SInteger n) = return $ Number $ SInteger n
floorNum (SRational r) = return $ Number $ SInteger $ floor r
floorNum (SReal d) = return $ Number $ SInteger $ floor d
floorNum (SComplex _) = throwError $ Default "floor not defined for complex"

ceilingNum :: SchemeNum -> ThrowsError LispVal
ceilingNum (SInteger n) = return $ Number $ SInteger n
ceilingNum (SRational r) = return $ Number $ SInteger $ ceiling r
ceilingNum (SReal d) = return $ Number $ SInteger $ ceiling d
ceilingNum (SComplex _) = throwError $ Default "ceiling not defined for complex"

truncateNum :: SchemeNum -> ThrowsError LispVal
truncateNum (SInteger n) = return $ Number $ SInteger n
truncateNum (SRational r) = return $ Number $ SInteger $ truncate r
truncateNum (SReal d) = return $ Number $ SInteger $ truncate d
truncateNum (SComplex _) = throwError $ Default "truncate not defined for complex"

roundNum :: SchemeNum -> ThrowsError LispVal
roundNum (SInteger n) = return $ Number $ SInteger n
roundNum (SRational r) = return $ Number $ SInteger $ round r
roundNum (SReal d) = return $ Number $ SInteger $ round d
roundNum (SComplex _) = throwError $ Default "round not defined for complex"

-- Rational operations

numeratorNum :: SchemeNum -> ThrowsError LispVal
numeratorNum (SInteger n) = return $ Number $ SInteger n
numeratorNum (SRational r) = return $ Number $ SInteger $ numerator r
numeratorNum (SReal d) = return $ Number $ SInteger $ numerator $ toRational d
numeratorNum (SComplex _) = throwError $ Default "numerator not defined for complex"

denominatorNum :: SchemeNum -> ThrowsError LispVal
denominatorNum (SInteger _) = return $ Number $ SInteger 1
denominatorNum (SRational r) = return $ Number $ SInteger $ denominator r
denominatorNum (SReal d) = return $ Number $ SInteger $ denominator $ toRational d
denominatorNum (SComplex _) = throwError $ Default "denominator not defined for complex"

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

-- Transcendental functions

unaryTranscendental :: (Double -> Double) -> [LispVal] -> ThrowsError LispVal
unaryTranscendental f [Number n] = return $ Number $ SReal $ f $ schemeNumToDouble n
unaryTranscendental _ args = throwError $ NumArgs 1 args

logNum :: [LispVal] -> ThrowsError LispVal
logNum [Number n] = return $ Number $ SReal $ log $ schemeNumToDouble n
logNum [Number n, Number base] = return $ Number $ SReal $ logBase (schemeNumToDouble base) (schemeNumToDouble n)
logNum args = throwError $ NumArgs 1 args

atanNum :: [LispVal] -> ThrowsError LispVal
atanNum [Number n] = return $ Number $ SReal $ atan $ schemeNumToDouble n
atanNum [Number y, Number x] = return $ Number $ SReal $ atan2 (schemeNumToDouble y) (schemeNumToDouble x)
atanNum args = throwError $ NumArgs 1 args

sqrtNum :: [LispVal] -> ThrowsError LispVal
sqrtNum [Number n] =
    let d = schemeNumToDouble n
    in if d < 0
        then return $ Number $ SComplex (0 :+ sqrt (-d))
        else return $ Number $ simplifyNum $ SReal $ sqrt d
sqrtNum args = throwError $ NumArgs 1 args

-- Complex number operations

makeRectangular :: [LispVal] -> ThrowsError LispVal
makeRectangular [Number r, Number i] =
    return $ Number $ simplifyNum $ SComplex (schemeNumToDouble r :+ schemeNumToDouble i)
makeRectangular args = throwError $ NumArgs 2 args

makePolar' :: [LispVal] -> ThrowsError LispVal
makePolar' [Number mag, Number ang] =
    return $ Number $ simplifyNum $ SComplex $ mkPolar (schemeNumToDouble mag) (schemeNumToDouble ang)
makePolar' args = throwError $ NumArgs 2 args

realPartNum :: SchemeNum -> ThrowsError LispVal
realPartNum (SComplex c) = return $ Number $ simplifyNum $ SReal $ realPart c
realPartNum n = return $ Number n

imagPartNum :: SchemeNum -> ThrowsError LispVal
imagPartNum (SComplex c) = return $ Number $ simplifyNum $ SReal $ imagPart c
imagPartNum _ = return $ Number $ SInteger 0

magnitudeNum :: SchemeNum -> ThrowsError LispVal
magnitudeNum (SComplex c) = return $ Number $ SReal $ magnitude c
magnitudeNum n = return $ Number $ SReal $ abs $ schemeNumToDouble n

angleNum :: SchemeNum -> ThrowsError LispVal
angleNum (SComplex c) = return $ Number $ SReal $ phase c
angleNum n
    | schemeNumToDouble n >= 0 = return $ Number $ SReal 0
    | otherwise = return $ Number $ SReal pi

-- Exactness conversions

exactToInexact :: SchemeNum -> ThrowsError LispVal
exactToInexact (SInteger n) = return $ Number $ SReal $ fromIntegral n
exactToInexact (SRational r) = return $ Number $ SReal $ fromRational r
exactToInexact n = return $ Number n

inexactToExact :: SchemeNum -> ThrowsError LispVal
inexactToExact (SReal d)
    | d == fromIntegral (round d :: Integer) = return $ Number $ SInteger $ round d
    | otherwise = return $ Number $ SRational $ toRational d
inexactToExact (SComplex c)
    | imagPart c == 0 = inexactToExact (SReal $ realPart c)
    | otherwise = throwError $ Default "cannot convert complex to exact"
inexactToExact n = return $ Number n

-- Boolean operations

notOp :: LispVal -> ThrowsError LispVal
notOp (Bool False) = return $ Bool True
notOp _ = return $ Bool False

isBool :: LispVal -> ThrowsError LispVal
isBool (Bool _) = return $ Bool True
isBool _ = return $ Bool False

-- Type predicates

isSymbol :: LispVal -> ThrowsError LispVal
isSymbol (Atom _) = return $ Bool True
isSymbol _ = return $ Bool False

isString :: LispVal -> ThrowsError LispVal
isString (String _) = return $ Bool True
isString _ = return $ Bool False

isChar :: LispVal -> ThrowsError LispVal
isChar (Char _) = return $ Bool True
isChar _ = return $ Bool False

isPair :: LispVal -> ThrowsError LispVal
isPair (List (_:_)) = return $ Bool True
isPair (DottedList _ _) = return $ Bool True
isPair _ = return $ Bool False

isList :: LispVal -> ThrowsError LispVal
isList (List _) = return $ Bool True
isList _ = return $ Bool False

isNull :: LispVal -> ThrowsError LispVal
isNull (List []) = return $ Bool True
isNull _ = return $ Bool False

isVector :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ Bool True
isVector _ = return $ Bool False

isProcedure :: LispVal -> ThrowsError LispVal
isProcedure (PrimitiveFunc _) = return $ Bool True
isProcedure (IOFunc _) = return $ Bool True
isProcedure (Func {}) = return $ Bool True
isProcedure _ = return $ Bool False

-- Symbol operations

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom s) = return $ String s
symbolToString notSym = throwError $ TypeMismatch "symbol" notSym

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol notStr = throwError $ TypeMismatch "string" notStr

-- String operations

stringLength :: LispVal -> ThrowsError LispVal
stringLength (String s) = return $ Number $ SInteger $ toInteger $ length s
stringLength notStr = throwError $ TypeMismatch "string" notStr

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number k] = do
    let idx = round $ schemeNumToDouble k
    if idx < 0 || idx >= toInteger (length s)
        then throwError $ OutOfRange "string-ref" idx
        else return $ Char $ s !! fromIntegral idx
stringRef [String _, notNum] = throwError $ TypeMismatch "integer" notNum
stringRef [notStr, _] = throwError $ TypeMismatch "string" notStr
stringRef args = throwError $ NumArgs 2 args

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend args = do
    strs <- mapM unpackStr args
    return $ String $ concat strs
  where
    unpackStr (String s) = return s
    unpackStr notStr = throwError $ TypeMismatch "string" notStr

substring :: [LispVal] -> ThrowsError LispVal
substring [String s, Number start', Number end'] = do
    let start = round $ schemeNumToDouble start'
    let end = round $ schemeNumToDouble end'
    if start < 0 || end > toInteger (length s) || start > end
        then throwError $ Default "substring: invalid indices"
        else return $ String $ take (fromIntegral $ end - start) $ drop (fromIntegral start) s
substring args = throwError $ NumArgs 3 args

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop op [String s1, String s2] = return $ Bool $ op s1 s2
strBoolBinop _ [String _, notStr] = throwError $ TypeMismatch "string" notStr
strBoolBinop _ [notStr, _] = throwError $ TypeMismatch "string" notStr
strBoolBinop _ args = throwError $ NumArgs 2 args

strCiBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strCiBoolBinop op [String s1, String s2] = return $ Bool $ op (map toLower s1) (map toLower s2)
strCiBoolBinop _ [String _, notStr] = throwError $ TypeMismatch "string" notStr
strCiBoolBinop _ [notStr, _] = throwError $ TypeMismatch "string" notStr
strCiBoolBinop _ args = throwError $ NumArgs 2 args

stringToList :: LispVal -> ThrowsError LispVal
stringToList (String s) = return $ List $ map Char s
stringToList notStr = throwError $ TypeMismatch "string" notStr

listToString :: LispVal -> ThrowsError LispVal
listToString (List cs) = do
    chars <- mapM unpackChar cs
    return $ String chars
  where
    unpackChar (Char c) = return c
    unpackChar notChar = throwError $ TypeMismatch "char" notChar
listToString notList = throwError $ TypeMismatch "list" notList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number k] = return $ String $ replicate (fromIntegral $ round $ schemeNumToDouble k) ' '
makeString [Number k, Char c] = return $ String $ replicate (fromIntegral $ round $ schemeNumToDouble k) c
makeString args = throwError $ NumArgs 1 args

stringFromChars :: [LispVal] -> ThrowsError LispVal
stringFromChars args = do
    chars <- mapM unpackChar args
    return $ String chars
  where
    unpackChar (Char c) = return c
    unpackChar notChar = throwError $ TypeMismatch "char" notChar

stringCopy :: LispVal -> ThrowsError LispVal
stringCopy (String s) = return $ String s
stringCopy notStr = throwError $ TypeMismatch "string" notStr

numberToString :: [LispVal] -> ThrowsError LispVal
numberToString [Number n] = return $ String $ show n
numberToString [Number n, Number radix] = do
    let r = round $ schemeNumToDouble radix
    let num = round $ schemeNumToDouble n
    return $ String $ showInRadix num r
  where
    showInRadix num 10 = show num
    showInRadix num 16 = showHex' num
    showInRadix num 8 = showOct' num
    showInRadix num 2 = showBin num
    showInRadix num _ = show num
    showHex' 0 = "0"
    showHex' n' = reverse $ showHex'' (abs n') ++ if n' < 0 then "-" else ""
    showHex'' 0 = ""
    showHex'' n' = hexDigit (n' `mod` 16) : showHex'' (n' `div` 16)
    hexDigit d = "0123456789abcdef" !! fromIntegral d
    showOct' 0 = "0"
    showOct' n' = reverse $ showOct'' (abs n') ++ if n' < 0 then "-" else ""
    showOct'' 0 = ""
    showOct'' n' = head (show (n' `mod` 8)) : showOct'' (n' `div` 8)
    showBin 0 = "0"
    showBin n' = reverse $ showBin' (abs n') ++ if n' < 0 then "-" else ""
    showBin' 0 = ""
    showBin' n' = (if n' `mod` 2 == 1 then '1' else '0') : showBin' (n' `div` 2)
numberToString args = throwError $ NumArgs 1 args

stringToNumber :: [LispVal] -> ThrowsError LispVal
stringToNumber [String s] = case readExpr s of
    Right (Number n) -> return $ Number n
    _ -> return $ Bool False
stringToNumber [String s, Number radix] = do
    let r = round $ schemeNumToDouble radix
    case r of
        10 -> stringToNumber [String s]
        16 -> case readExpr ("#x" ++ s) of
            Right (Number n) -> return $ Number n
            _ -> return $ Bool False
        8 -> case readExpr ("#o" ++ s) of
            Right (Number n) -> return $ Number n
            _ -> return $ Bool False
        2 -> case readExpr ("#b" ++ s) of
            Right (Number n) -> return $ Number n
            _ -> return $ Bool False
        _ -> return $ Bool False
stringToNumber args = throwError $ NumArgs 1 args

-- Character operations

charBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charBoolBinop op [Char c1, Char c2] = return $ Bool $ op c1 c2
charBoolBinop _ [Char _, notChar] = throwError $ TypeMismatch "char" notChar
charBoolBinop _ [notChar, _] = throwError $ TypeMismatch "char" notChar
charBoolBinop _ args = throwError $ NumArgs 2 args

charCiBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charCiBoolBinop op [Char c1, Char c2] = return $ Bool $ op (toLower c1) (toLower c2)
charCiBoolBinop _ [Char _, notChar] = throwError $ TypeMismatch "char" notChar
charCiBoolBinop _ [notChar, _] = throwError $ TypeMismatch "char" notChar
charCiBoolBinop _ args = throwError $ NumArgs 2 args

charToInteger :: LispVal -> ThrowsError LispVal
charToInteger (Char c) = return $ Number $ SInteger $ toInteger $ fromEnum c
charToInteger notChar = throwError $ TypeMismatch "char" notChar

integerToChar :: LispVal -> ThrowsError LispVal
integerToChar (Number n) = return $ Char $ toEnum $ fromIntegral $ round $ schemeNumToDouble n
integerToChar notNum = throwError $ TypeMismatch "integer" notNum

charUpcase :: LispVal -> ThrowsError LispVal
charUpcase (Char c) = return $ Char $ toUpper c
charUpcase notChar = throwError $ TypeMismatch "char" notChar

charDowncase :: LispVal -> ThrowsError LispVal
charDowncase (Char c) = return $ Char $ toLower c
charDowncase notChar = throwError $ TypeMismatch "char" notChar

charAlphabetic :: LispVal -> ThrowsError LispVal
charAlphabetic (Char c) = return $ Bool $ isAlpha c
charAlphabetic notChar = throwError $ TypeMismatch "char" notChar

charNumeric :: LispVal -> ThrowsError LispVal
charNumeric (Char c) = return $ Bool $ isDigit c
charNumeric notChar = throwError $ TypeMismatch "char" notChar

charWhitespace :: LispVal -> ThrowsError LispVal
charWhitespace (Char c) = return $ Bool $ isSpace c
charWhitespace notChar = throwError $ TypeMismatch "char" notChar

charUpperCase :: LispVal -> ThrowsError LispVal
charUpperCase (Char c) = return $ Bool $ isUpper c
charUpperCase notChar = throwError $ TypeMismatch "char" notChar

charLowerCase :: LispVal -> ThrowsError LispVal
charLowerCase (Char c) = return $ Bool $ isLower c
charLowerCase notChar = throwError $ TypeMismatch "char" notChar

-- List operations

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xl] = return $ DottedList (x : xs) xl
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

list' :: [LispVal] -> ThrowsError LispVal
list' = return . List

listLength :: [LispVal] -> ThrowsError LispVal
listLength [List xs] = return $ Number $ SInteger $ toInteger $ length xs
listLength [badArg] = throwError $ TypeMismatch "list" badArg
listLength badArgs = throwError $ NumArgs 1 badArgs

appendLists :: [LispVal] -> ThrowsError LispVal
appendLists [] = return $ List []
appendLists [x] = return x
appendLists (List x : rest) = do
    result <- appendLists rest
    case result of
        List rest' -> return $ List $ x ++ rest'
        _ -> throwError $ TypeMismatch "list" result
appendLists (badArg : _) = throwError $ TypeMismatch "list" badArg

reverseList :: [LispVal] -> ThrowsError LispVal
reverseList [List xs] = return $ List $ reverse xs
reverseList [badArg] = throwError $ TypeMismatch "list" badArg
reverseList badArgs = throwError $ NumArgs 1 badArgs

listRef :: [LispVal] -> ThrowsError LispVal
listRef [List xs, Number k] = do
    let idx = round $ schemeNumToDouble k
    if idx < 0 || idx >= toInteger (length xs)
        then throwError $ OutOfRange "list-ref" idx
        else return $ xs !! fromIntegral idx
listRef [badArg, Number _] = throwError $ TypeMismatch "list" badArg
listRef [_, badArg] = throwError $ TypeMismatch "integer" badArg
listRef badArgs = throwError $ NumArgs 2 badArgs

listTail :: [LispVal] -> ThrowsError LispVal
listTail [List xs, Number k] = do
    let idx = round $ schemeNumToDouble k
    if idx < 0 || idx > toInteger (length xs)
        then throwError $ OutOfRange "list-tail" idx
        else return $ List $ drop (fromIntegral idx) xs
listTail [badArg, Number _] = throwError $ TypeMismatch "list" badArg
listTail [_, badArg] = throwError $ TypeMismatch "integer" badArg
listTail badArgs = throwError $ NumArgs 2 badArgs

memq :: [LispVal] -> ThrowsError LispVal
memq [_, List []] = return $ Bool False
memq [obj, List (x:xs)] = do
    result <- eqv [obj, x]
    case result of
        Bool True -> return $ List (x:xs)
        _ -> memq [obj, List xs]
memq [_, badArg] = throwError $ TypeMismatch "list" badArg
memq badArgs = throwError $ NumArgs 2 badArgs

memv :: [LispVal] -> ThrowsError LispVal
memv = memq

member :: [LispVal] -> ThrowsError LispVal
member [_, List []] = return $ Bool False
member [obj, List (x:xs)] = do
    result <- equal [obj, x]
    case result of
        Bool True -> return $ List (x:xs)
        _ -> member [obj, List xs]
member [_, badArg] = throwError $ TypeMismatch "list" badArg
member badArgs = throwError $ NumArgs 2 badArgs

assq :: [LispVal] -> ThrowsError LispVal
assq [_, List []] = return $ Bool False
assq [obj, List (List (k:v):rest)] = do
    result <- eqv [obj, k]
    case result of
        Bool True -> return $ List (k:v)
        _ -> assq [obj, List rest]
assq [obj, List (DottedList (k:_) _ : rest)] = do
    result <- eqv [obj, k]
    case result of
        Bool True -> return $ Bool True
        _ -> assq [obj, List rest]
assq [_, List (_:rest)] = assq [Atom "", List rest]
assq [_, badArg] = throwError $ TypeMismatch "list" badArg
assq badArgs = throwError $ NumArgs 2 badArgs

assv :: [LispVal] -> ThrowsError LispVal
assv = assq

assoc' :: [LispVal] -> ThrowsError LispVal
assoc' [_, List []] = return $ Bool False
assoc' [obj, List (List (k:v):rest)] = do
    result <- equal [obj, k]
    case result of
        Bool True -> return $ List (k:v)
        _ -> assoc' [obj, List rest]
assoc' [_, List (_:rest)] = assoc' [Atom "", List rest]
assoc' [_, badArg] = throwError $ TypeMismatch "list" badArg
assoc' badArgs = throwError $ NumArgs 2 badArgs

-- Vector operations

vector :: [LispVal] -> ThrowsError LispVal
vector args = return $ Vector $ listArray (0, length args - 1) args

makeVector :: [LispVal] -> ThrowsError LispVal
makeVector [Number k] = do
    let size = fromIntegral $ round $ schemeNumToDouble k
    return $ Vector $ listArray (0, size - 1) (replicate size (Number $ SInteger 0))
makeVector [Number k, fill] = do
    let size = fromIntegral $ round $ schemeNumToDouble k
    return $ Vector $ listArray (0, size - 1) (replicate size fill)
makeVector args = throwError $ NumArgs 1 args

vectorLength :: [LispVal] -> ThrowsError LispVal
vectorLength [Vector v] = let (_, hi) = bounds v in return $ Number $ SInteger $ toInteger $ hi + 1
vectorLength [badArg] = throwError $ TypeMismatch "vector" badArg
vectorLength badArgs = throwError $ NumArgs 1 badArgs

vectorRef :: [LispVal] -> ThrowsError LispVal
vectorRef [Vector v, Number k] = do
    let idx = round $ schemeNumToDouble k
    let (lo, hi) = bounds v
    if idx < toInteger lo || idx > toInteger hi
        then throwError $ OutOfRange "vector-ref" idx
        else return $ v ! fromIntegral idx
vectorRef [Vector _, badArg] = throwError $ TypeMismatch "integer" badArg
vectorRef [badArg, _] = throwError $ TypeMismatch "vector" badArg
vectorRef badArgs = throwError $ NumArgs 2 badArgs

vectorSet :: [LispVal] -> ThrowsError LispVal
vectorSet [Vector v, Number k, val] = do
    let idx = round $ schemeNumToDouble k
    let (lo, hi) = bounds v
    if idx < toInteger lo || idx > toInteger hi
        then throwError $ OutOfRange "vector-set!" idx
        else return $ Vector $ v // [(fromIntegral idx, val)]
vectorSet [Vector _, badArg, _] = throwError $ TypeMismatch "integer" badArg
vectorSet [badArg, _, _] = throwError $ TypeMismatch "vector" badArg
vectorSet badArgs = throwError $ NumArgs 3 badArgs

vectorToList :: [LispVal] -> ThrowsError LispVal
vectorToList [Vector v] = return $ List $ elems v
vectorToList [badArg] = throwError $ TypeMismatch "vector" badArg
vectorToList badArgs = throwError $ NumArgs 1 badArgs

listToVector :: [LispVal] -> ThrowsError LispVal
listToVector [List xs] = return $ Vector $ listArray (0, length xs - 1) xs
listToVector [badArg] = throwError $ TypeMismatch "list" badArg
listToVector badArgs = throwError $ NumArgs 1 badArgs

-- Equivalence predicates

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool b1, Bool b2] = return $ Bool $ b1 == b2
eqv [Number n1, Number n2] = return $ Bool $ n1 == n2
eqv [String s1, String s2] = return $ Bool $ s1 == s2
eqv [Char c1, Char c2] = return $ Bool $ c1 == c2
eqv [Atom a1, Atom a2] = return $ Bool $ a1 == a2
eqv [List [], List []] = return $ Bool True
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [List xs, List ys] =
    if length xs /= length ys
        then return $ Bool False
        else do
            results <- zipWithM (\x y -> equal [x, y]) xs ys
            return $ Bool $ all isBoolTrue results
  where
    isBoolTrue (Bool True) = True
    isBoolTrue _ = False
equal [DottedList xs x, DottedList ys y] = do
    headResult <- equal [List xs, List ys]
    tailResult <- equal [x, y]
    case (headResult, tailResult) of
        (Bool headEq, Bool tailEq) -> return $ Bool $ headEq && tailEq
        _ -> return $ Bool False
equal [Vector v1, Vector v2] = equal [List $ elems v1, List $ elems v2]
equal args = eqv args

-- Error

errorProc :: [LispVal] -> ThrowsError LispVal
errorProc [String msg] = throwError $ Default msg
errorProc [msg] = throwError $ Default $ show msg
errorProc args = throwError $ Default $ "Error: " ++ unwords (map show args)

-- IO Primitives

displayProc :: [LispVal] -> IOThrowsError LispVal
displayProc [val] = displayProc [val, Port stdout]
displayProc [String s, Port h] = liftIO (hPutStr h s) >> return Void
displayProc [val, Port h] = liftIO (hPutStr h $ showValForDisplay val) >> return Void
displayProc [_, badArg] = throwError $ TypeMismatch "port" badArg
displayProc badArgs = throwError $ NumArgs 1 badArgs

showValForDisplay :: LispVal -> String
showValForDisplay (String s) = s
showValForDisplay v = show v

newlineProc :: [LispVal] -> IOThrowsError LispVal
newlineProc [] = newlineProc [Port stdout]
newlineProc [Port h] = liftIO (hPutStr h "\n") >> return Void
newlineProc [badArg] = throwError $ TypeMismatch "port" badArg
newlineProc badArgs = throwError $ NumArgs 0 badArgs

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [val] = writeProc [val, Port stdout]
writeProc [val, Port h] = liftIO (hPutStr h $ show val) >> return Void
writeProc [_, badArg] = throwError $ TypeMismatch "port" badArg
writeProc badArgs = throwError $ NumArgs 1 badArgs

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port h] = do
    eof <- liftIO $ hIsEOF h
    if eof
        then return EOF
        else do
            input <- liftIO $ hGetLine h
            liftThrows $ readExpr input
readProc [badArg] = throwError $ TypeMismatch "port" badArg
readProc badArgs = throwError $ NumArgs 0 badArgs

load :: [LispVal] -> IOThrowsError LispVal
load [String filename] = do
    contents <- liftIO $ readFile filename
    liftThrows $ readExprList contents >>= return . List
load [badArg] = throwError $ TypeMismatch "string" badArg
load badArgs = throwError $ NumArgs 1 badArgs

-- Port predicates

isPort :: LispVal -> ThrowsError LispVal
isPort (Port _) = return $ Bool True
isPort _ = return $ Bool False

isInputPort :: LispVal -> ThrowsError LispVal
isInputPort (Port _) = return $ Bool True  -- Simplified: all ports can be input
isInputPort _ = return $ Bool False

isOutputPort :: LispVal -> ThrowsError LispVal
isOutputPort (Port _) = return $ Bool True  -- Simplified: all ports can be output
isOutputPort _ = return $ Bool False

isEOFObject :: LispVal -> ThrowsError LispVal
isEOFObject EOF = return $ Bool True
isEOFObject _ = return $ Bool False

-- Port I/O operations

openInputFile :: [LispVal] -> IOThrowsError LispVal
openInputFile [String filename] = do
    result <- liftIO $ try (openFile filename ReadMode)
    case result of
        Left e -> throwError $ Default $ "Cannot open file: " ++ show (e :: IOException)
        Right h -> return $ Port h
openInputFile [badArg] = throwError $ TypeMismatch "string" badArg
openInputFile badArgs = throwError $ NumArgs 1 badArgs

openOutputFile :: [LispVal] -> IOThrowsError LispVal
openOutputFile [String filename] = do
    result <- liftIO $ try (openFile filename WriteMode)
    case result of
        Left e -> throwError $ Default $ "Cannot open file: " ++ show (e :: IOException)
        Right h -> return $ Port h
openOutputFile [badArg] = throwError $ TypeMismatch "string" badArg
openOutputFile badArgs = throwError $ NumArgs 1 badArgs

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port h] = liftIO (hClose h) >> return Void
closePort [badArg] = throwError $ TypeMismatch "port" badArg
closePort badArgs = throwError $ NumArgs 1 badArgs

currentInputPort :: [LispVal] -> IOThrowsError LispVal
currentInputPort [] = return $ Port stdin
currentInputPort badArgs = throwError $ NumArgs 0 badArgs

currentOutputPort :: [LispVal] -> IOThrowsError LispVal
currentOutputPort [] = return $ Port stdout
currentOutputPort badArgs = throwError $ NumArgs 0 badArgs

-- Character I/O

readChar :: [LispVal] -> IOThrowsError LispVal
readChar [] = readChar [Port stdin]
readChar [Port h] = do
    eof <- liftIO $ hIsEOF h
    if eof
        then return EOF
        else do
            c <- liftIO $ hGetChar h
            return $ Char c
readChar [badArg] = throwError $ TypeMismatch "port" badArg
readChar badArgs = throwError $ NumArgs 1 badArgs

peekChar :: [LispVal] -> IOThrowsError LispVal
peekChar [] = peekChar [Port stdin]
peekChar [Port h] = do
    eof <- liftIO $ hIsEOF h
    if eof
        then return EOF
        else do
            c <- liftIO $ hLookAhead h
            return $ Char c
peekChar [badArg] = throwError $ TypeMismatch "port" badArg
peekChar badArgs = throwError $ NumArgs 1 badArgs

writeChar :: [LispVal] -> IOThrowsError LispVal
writeChar [Char c] = writeChar [Char c, Port stdout]
writeChar [Char c, Port h] = liftIO (hPutChar h c) >> return Void
writeChar [Char _, badArg] = throwError $ TypeMismatch "port" badArg
writeChar [badArg, _] = throwError $ TypeMismatch "char" badArg
writeChar badArgs = throwError $ NumArgs 1 badArgs

charReady :: [LispVal] -> IOThrowsError LispVal
charReady [] = charReady [Port stdin]
charReady [Port h] = do
    ready <- liftIO $ hReady h
    return $ Bool ready
charReady [badArg] = throwError $ TypeMismatch "port" badArg
charReady badArgs = throwError $ NumArgs 1 badArgs

-- call-with-*-file (simplified - doesn't handle continuations properly)

callWithInputFile :: [LispVal] -> IOThrowsError LispVal
callWithInputFile [String _, _] =
    throwError $ Default "call-with-input-file: requires apply (implement in Eval)"
callWithInputFile badArgs = throwError $ NumArgs 2 badArgs

callWithOutputFile :: [LispVal] -> IOThrowsError LispVal
callWithOutputFile [String _, _] =
    throwError $ Default "call-with-output-file: requires apply (implement in Eval)"
callWithOutputFile badArgs = throwError $ NumArgs 2 badArgs

-- String mutation

stringSet :: [LispVal] -> ThrowsError LispVal
stringSet [String s, Number k, Char c] = do
    let idx = fromIntegral $ round $ schemeNumToDouble k
    if idx < 0 || idx >= length s
        then throwError $ OutOfRange "string-set!" (toInteger idx)
        else do
            let (before, _:after) = splitAt idx s
            return $ String $ before ++ [c] ++ after
stringSet [String _, Number _, badArg] = throwError $ TypeMismatch "char" badArg
stringSet [String _, badArg, _] = throwError $ TypeMismatch "integer" badArg
stringSet [badArg, _, _] = throwError $ TypeMismatch "string" badArg
stringSet badArgs = throwError $ NumArgs 3 badArgs

stringFill :: [LispVal] -> ThrowsError LispVal
stringFill [String s, Char c] = return $ String $ replicate (length s) c
stringFill [String _, badArg] = throwError $ TypeMismatch "char" badArg
stringFill [badArg, _] = throwError $ TypeMismatch "string" badArg
stringFill badArgs = throwError $ NumArgs 2 badArgs

-- Vector mutation

vectorFill :: [LispVal] -> ThrowsError LispVal
vectorFill [Vector v, fill] = do
    let (lo, hi) = bounds v
    return $ Vector $ listArray (lo, hi) (replicate (hi - lo + 1) fill)
vectorFill [badArg, _] = throwError $ TypeMismatch "vector" badArg
vectorFill badArgs = throwError $ NumArgs 2 badArgs

-- Pair mutation (returns new pair - true mutation requires mutable cells)

-- | Create a mutable pair (cons cell that can be mutated with set-car!/set-cdr!)
mcons :: [LispVal] -> IOThrowsError LispVal
mcons [car', cdr'] = do
    carRef <- liftIO $ newIORef car'
    cdrRef <- liftIO $ newIORef cdr'
    return $ MutablePair carRef cdrRef
mcons badArgs = throwError $ NumArgs 2 badArgs

-- | Get car of a mutable pair
mcar :: [LispVal] -> IOThrowsError LispVal
mcar [MutablePair carRef _] = liftIO $ readIORef carRef
mcar [badArg] = throwError $ TypeMismatch "mutable-pair" badArg
mcar badArgs = throwError $ NumArgs 1 badArgs

-- | Get cdr of a mutable pair
mcdr :: [LispVal] -> IOThrowsError LispVal
mcdr [MutablePair _ cdrRef] = liftIO $ readIORef cdrRef
mcdr [badArg] = throwError $ TypeMismatch "mutable-pair" badArg
mcdr badArgs = throwError $ NumArgs 1 badArgs

-- | Check if value is a mutable pair
mpairQ :: [LispVal] -> IOThrowsError LispVal
mpairQ [MutablePair _ _] = return $ Bool True
mpairQ [_] = return $ Bool False
mpairQ badArgs = throwError $ NumArgs 1 badArgs

-- | set-car! - mutates the car of a mutable pair
-- For immutable pairs, returns a new pair (non-R5RS but preserves backwards compat)
setCar :: [LispVal] -> IOThrowsError LispVal
setCar [MutablePair carRef _, val] = do
    liftIO $ writeIORef carRef val
    return Void  -- R5RS: set-car! returns unspecified value
setCar [List (_:xs), val] = return $ List (val:xs)  -- Non-mutating fallback
setCar [DottedList (_:xs) t, val] = return $ DottedList (val:xs) t
setCar [List [], _] = throwError $ TypeMismatch "pair" (List [])
setCar [badArg, _] = throwError $ TypeMismatch "pair" badArg
setCar badArgs = throwError $ NumArgs 2 badArgs

-- | set-cdr! - mutates the cdr of a mutable pair
-- For immutable pairs, returns a new pair (non-R5RS but preserves backwards compat)
setCdr :: [LispVal] -> IOThrowsError LispVal
setCdr [MutablePair _ cdrRef, val] = do
    liftIO $ writeIORef cdrRef val
    return Void  -- R5RS: set-cdr! returns unspecified value
setCdr [List (x:_), List ys] = return $ List (x:ys)  -- Non-mutating fallback
setCdr [List (x:_), val] = return $ DottedList [x] val
setCdr [DottedList (x:_) _, List ys] = return $ List (x:ys)
setCdr [DottedList (x:_) _, val] = return $ DottedList [x] val
setCdr [List [], _] = throwError $ TypeMismatch "pair" (List [])
setCdr [badArg, _] = throwError $ TypeMismatch "pair" badArg
setCdr badArgs = throwError $ NumArgs 2 badArgs

-- Values - for multiple return values (simplified implementation)
-- In R5RS, values returns multiple values; we wrap them in a special list
valuesProc :: [LispVal] -> ThrowsError LispVal
valuesProc [val] = return val  -- Single value returned as-is
valuesProc vals = return $ DottedList [Atom "#values"] (List vals)

-- call-with-values - calls consumer with producer's values
callWithValues :: [LispVal] -> IOThrowsError LispVal
callWithValues [_, _] =
    throwError $ Default "call-with-values: requires apply (implement in Eval)"
callWithValues badArgs = throwError $ NumArgs 2 badArgs

-- Environment procedures
schemeReportEnv :: [LispVal] -> IOThrowsError LispVal
schemeReportEnv [Number _] = do
    -- Returns a specifier for R5RS environment
    -- For simplicity, we return a symbol that eval can recognize
    return $ List [Atom "scheme-report-environment", Number (SInteger 5)]
schemeReportEnv [badArg] = throwError $ TypeMismatch "integer" badArg
schemeReportEnv badArgs = throwError $ NumArgs 1 badArgs

nullEnvProc :: [LispVal] -> IOThrowsError LispVal
nullEnvProc [Number _] = do
    -- Returns a specifier for null environment (only special forms)
    return $ List [Atom "null-environment", Number (SInteger 5)]
nullEnvProc [badArg] = throwError $ TypeMismatch "integer" badArg
nullEnvProc badArgs = throwError $ NumArgs 1 badArgs

interactionEnv :: [LispVal] -> IOThrowsError LispVal
interactionEnv [] = return $ List [Atom "interaction-environment"]
interactionEnv badArgs = throwError $ NumArgs 0 badArgs

-- with-input-from-file and with-output-to-file (simplified - require eval)
withInputFromFile :: [LispVal] -> IOThrowsError LispVal
withInputFromFile [String _, _] =
    throwError $ Default "with-input-from-file: requires apply (implement in Eval)"
withInputFromFile badArgs = throwError $ NumArgs 2 badArgs

withOutputToFile :: [LispVal] -> IOThrowsError LispVal
withOutputToFile [String _, _] =
    throwError $ Default "with-output-to-file: requires apply (implement in Eval)"
withOutputToFile badArgs = throwError $ NumArgs 2 badArgs

-- Transcript procedures (optional in R5RS - implemented as no-ops)
transcriptOn :: [LispVal] -> IOThrowsError LispVal
transcriptOn [String _] = return Void  -- No-op
transcriptOn badArgs = throwError $ NumArgs 1 badArgs

transcriptOff :: [LispVal] -> IOThrowsError LispVal
transcriptOff [] = return Void  -- No-op
transcriptOff badArgs = throwError $ NumArgs 0 badArgs
