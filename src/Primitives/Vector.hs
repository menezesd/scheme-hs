{-# LANGUAGE FlexibleContexts #-}

-- | Vector primitives for R5RS Scheme
--
-- This module implements vector operations including:
--
-- * Vector construction
-- * Vector access and mutation
-- * Vector/list conversion
module Primitives.Vector
    ( -- * Primitive bindings
      vectorPrimitives
      -- * Vector operations
    , vector
    , makeVector
    , vectorLength
    , vectorRef
    , vectorSet
    , vectorToList
    , listToVector
    , vectorFill
    ) where

import Types
import Control.Monad.Except
import Data.Array (listArray, bounds, elems, (!), (//))

-- | All vector primitive functions
vectorPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
vectorPrimitives =
    [ ("vector", vector)
    , ("make-vector", makeVector)
    , ("vector-length", vectorLength)
    , ("vector-ref", vectorRef)
    , ("vector-set!", vectorSet)
    , ("vector->list", vectorToList)
    , ("list->vector", listToVector)
    , ("vector-fill!", vectorFill)
    ]

-- | Create a vector from arguments
vector :: [LispVal] -> ThrowsError LispVal
vector args = return $ Vector $ listArray (0, length args - 1) args

-- | Create a vector of given size, optionally filled with a value
makeVector :: [LispVal] -> ThrowsError LispVal
makeVector [Number k] = do
    let size = fromIntegral $ round $ schemeNumToDouble k
    return $ Vector $ listArray (0, size - 1) (replicate size (Number $ SInteger 0))
makeVector [Number k, fill] = do
    let size = fromIntegral $ round $ schemeNumToDouble k
    return $ Vector $ listArray (0, size - 1) (replicate size fill)
makeVector args = throwError $ NumArgs 1 args

-- | Get the length of a vector
vectorLength :: [LispVal] -> ThrowsError LispVal
vectorLength [Vector v] = let (_, hi) = bounds v in return $ Number $ SInteger $ toInteger $ hi + 1
vectorLength [badArg] = throwError $ TypeMismatch "vector" badArg
vectorLength badArgs = throwError $ NumArgs 1 badArgs

-- | Get element at index
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

-- | Set element at index (returns new vector)
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

-- | Convert vector to list
vectorToList :: [LispVal] -> ThrowsError LispVal
vectorToList [Vector v] = return $ List $ elems v
vectorToList [badArg] = throwError $ TypeMismatch "vector" badArg
vectorToList badArgs = throwError $ NumArgs 1 badArgs

-- | Convert list to vector
listToVector :: [LispVal] -> ThrowsError LispVal
listToVector [List xs] = return $ Vector $ listArray (0, length xs - 1) xs
listToVector [badArg] = throwError $ TypeMismatch "list" badArg
listToVector badArgs = throwError $ NumArgs 1 badArgs

-- | Fill vector with a value (returns new vector)
vectorFill :: [LispVal] -> ThrowsError LispVal
vectorFill [Vector v, fill] = do
    let (lo, hi) = bounds v
    return $ Vector $ listArray (lo, hi) (replicate (hi - lo + 1) fill)
vectorFill [badArg, _] = throwError $ TypeMismatch "vector" badArg
vectorFill badArgs = throwError $ NumArgs 2 badArgs
