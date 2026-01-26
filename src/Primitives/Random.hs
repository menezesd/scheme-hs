{-# LANGUAGE BangPatterns #-}

-- | SRFI-27 Random Number Generation
--
-- This module implements SRFI-27 compatible random number generation using
-- the xoshiro256** algorithm, a high-quality pseudorandom number generator.
--
-- Provides:
--
-- * @random-integer n@ - uniform integer in [0, n)
-- * @random-real@ - uniform double in [0, 1)
-- * @random-seed!@ - seed the generator
-- * @make-random-source@ - create a new random source
-- * @random-source?@ - predicate for random sources
module Primitives.Random
    ( randomPrimitives
    , initDefaultRandomSource
    ) where

import Types
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Word (Word64)
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as R

-- | Global default random source
{-# NOINLINE defaultRandomSource #-}
defaultRandomSource :: IORef (Maybe RandomSource)
defaultRandomSource = unsafePerformIO $ newIORef Nothing

-- | Initialize the default random source
initDefaultRandomSource :: IO RandomSource
initDefaultRandomSource = do
    existing <- readIORef defaultRandomSource
    case existing of
        Just rs -> return rs
        Nothing -> do
            rs <- makeRandomSourceIO
            writeIORef defaultRandomSource (Just rs)
            return rs

-- | Create a new random source with random seed
makeRandomSourceIO :: IO RandomSource
makeRandomSourceIO = do
    -- Use system random to seed
    s0 <- R.randomIO :: IO Word64
    s1 <- R.randomIO :: IO Word64
    s2 <- R.randomIO :: IO Word64
    s3 <- R.randomIO :: IO Word64
    -- Ensure non-zero state
    let s0' = if s0 == 0 && s1 == 0 && s2 == 0 && s3 == 0 then 1 else s0
    stateRef <- newIORef (s0', s1, s2, s3)
    return $ RandomSource stateRef

-- | Seed a random source from an integer
seedRandomSource :: RandomSource -> Integer -> IO ()
seedRandomSource rs seed = do
    let s0 = fromIntegral (seed `xor` 0x5a17a92c8f8e3b1d) :: Word64
        s1 = fromIntegral ((seed `shiftR` 64) `xor` 0x3c4a89d2e7f5c6b0) :: Word64
        s2 = fromIntegral ((seed * 0x9e3779b97f4a7c15) `xor` 0x1b2e4f8d9c3a5e7f) :: Word64
        s3 = fromIntegral ((seed * 0x6a09e667f3bcc908) `xor` 0x8b4d5c6e7f8a9b0c) :: Word64
        -- Ensure non-zero
        s0' = if s0 == 0 && s1 == 0 && s2 == 0 && s3 == 0 then 1 else s0
    writeIORef (rsState rs) (s0', s1, s2, s3)

-- | Rotate left
rotl :: Word64 -> Int -> Word64
rotl x k = (x `shiftL` k) .|. (x `shiftR` (64 - k))

-- | xoshiro256** next step - returns random Word64 and advances state
xoshiro256ss :: RandomSource -> IO Word64
xoshiro256ss rs = do
    (s0, s1, s2, s3) <- readIORef (rsState rs)
    let result = rotl (s1 * 5) 7 * 9
        t = s1 `shiftL` 17
        s2' = s2 `xor` s0
        s3' = s3 `xor` s1
        s1' = s1 `xor` s2'
        s0' = s0 `xor` s3'
        s2'' = s2' `xor` t
        s3'' = rotl s3' 45
    writeIORef (rsState rs) (s0', s1', s2'', s3'')
    return result

-- | Generate random integer in [0, n) using rejection sampling (unbiased)
randomIntegerIO :: RandomSource -> Integer -> IO Integer
randomIntegerIO rs n
    | n <= 0 = return 0
    | n <= toInteger (maxBound :: Word64) = do
        -- Fast path for small n
        let bound = fromIntegral n :: Word64
            threshold = (maxBound - bound + 1) `mod` bound
        let loop = do
                r <- xoshiro256ss rs
                if r >= threshold
                    then return $ toInteger (r `mod` bound)
                    else loop
        loop
    | otherwise = do
        -- Large n: generate enough random bits
        let bits = ceiling (logBase 2 (fromIntegral n) :: Double) :: Int
            words = (bits + 63) `div` 64
        let loop = do
                ws <- mapM (const $ xoshiro256ss rs) [1..words]
                let r = foldr (\w acc -> (acc `shiftL` 64) .|. toInteger w) 0 ws
                    r' = r `mod` (2 ^ bits)
                if r' < n
                    then return r'
                    else loop
        loop

-- | Generate random real in [0, 1) using 53 bits of randomness
randomRealIO :: RandomSource -> IO Double
randomRealIO rs = do
    r <- xoshiro256ss rs
    -- Use top 53 bits for IEEE double precision
    let bits = r `shiftR` 11
    return $ fromIntegral bits / 9007199254740992.0  -- 2^53

-- | All random primitives
randomPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
randomPrimitives =
    [ ("random-integer", randomInteger)
    , ("random-real", randomReal)
    , ("random-seed!", randomSeed)
    , ("make-random-source", makeRandomSource)
    , ("random-source?", isRandomSource)
    , ("random-source-randomize!", randomSourceRandomize)
    , ("random-source-pseudo-randomize!", randomSourcePseudoRandomize)
    ]

-- | Get or create the default random source
getDefaultSource :: IOThrowsError RandomSource
getDefaultSource = liftIO initDefaultRandomSource

-- | random-integer n - returns uniform integer in [0, n)
randomInteger :: [LispVal] -> IOThrowsError LispVal
randomInteger [Number (SInteger n)] = do
    rs <- getDefaultSource
    r <- liftIO $ randomIntegerIO rs n
    return $ Number $ SInteger r
randomInteger [Number n] = do
    let n' = round $ schemeNumToDouble n
    rs <- getDefaultSource
    r <- liftIO $ randomIntegerIO rs n'
    return $ Number $ SInteger r
randomInteger [badArg] = throwError $ TypeMismatch "positive integer" badArg
randomInteger badArgs = throwError $ NumArgs 1 badArgs

-- | random-real - returns uniform double in [0, 1)
randomReal :: [LispVal] -> IOThrowsError LispVal
randomReal [] = do
    rs <- getDefaultSource
    r <- liftIO $ randomRealIO rs
    return $ Number $ SReal r
randomReal badArgs = throwError $ NumArgs 0 badArgs

-- | random-seed! n - seed the default random source
randomSeed :: [LispVal] -> IOThrowsError LispVal
randomSeed [Number (SInteger n)] = do
    rs <- getDefaultSource
    liftIO $ seedRandomSource rs n
    return Void
randomSeed [Number n] = do
    let n' = round $ schemeNumToDouble n
    rs <- getDefaultSource
    liftIO $ seedRandomSource rs n'
    return Void
randomSeed [badArg] = throwError $ TypeMismatch "integer" badArg
randomSeed badArgs = throwError $ NumArgs 1 badArgs

-- | make-random-source - create a new random source
makeRandomSource :: [LispVal] -> IOThrowsError LispVal
makeRandomSource [] = do
    rs <- liftIO makeRandomSourceIO
    return $ RandomSourceVal rs
makeRandomSource badArgs = throwError $ NumArgs 0 badArgs

-- | random-source? - check if value is a random source
isRandomSource :: [LispVal] -> IOThrowsError LispVal
isRandomSource [RandomSourceVal _] = return $ Bool True
isRandomSource [_] = return $ Bool False
isRandomSource badArgs = throwError $ NumArgs 1 badArgs

-- | random-source-randomize! - re-seed from system entropy
randomSourceRandomize :: [LispVal] -> IOThrowsError LispVal
randomSourceRandomize [RandomSourceVal rs] = do
    s0 <- liftIO (R.randomIO :: IO Word64)
    s1 <- liftIO (R.randomIO :: IO Word64)
    s2 <- liftIO (R.randomIO :: IO Word64)
    s3 <- liftIO (R.randomIO :: IO Word64)
    let s0' = if s0 == 0 && s1 == 0 && s2 == 0 && s3 == 0 then 1 else s0
    liftIO $ writeIORef (rsState rs) (s0', s1, s2, s3)
    return Void
randomSourceRandomize [badArg] = throwError $ TypeMismatch "random-source" badArg
randomSourceRandomize badArgs = throwError $ NumArgs 1 badArgs

-- | random-source-pseudo-randomize! i j - seed deterministically from i, j
randomSourcePseudoRandomize :: [LispVal] -> IOThrowsError LispVal
randomSourcePseudoRandomize [RandomSourceVal rs, Number (SInteger i), Number (SInteger j)] = do
    let seed = i * 0x9e3779b97f4a7c15 + j * 0x6a09e667f3bcc908
    liftIO $ seedRandomSource rs seed
    return Void
randomSourcePseudoRandomize [_, Number _, badArg] = throwError $ TypeMismatch "integer" badArg
randomSourcePseudoRandomize [_, badArg, _] = throwError $ TypeMismatch "integer" badArg
randomSourcePseudoRandomize [badArg, _, _] = throwError $ TypeMismatch "random-source" badArg
randomSourcePseudoRandomize badArgs = throwError $ NumArgs 3 badArgs
