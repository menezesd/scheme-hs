{-# LANGUAGE FlexibleContexts #-}

-- | List primitives for R5RS Scheme
--
-- This module implements list operations including:
--
-- * Basic list construction and access (car, cdr, cons)
-- * List predicates and utilities
-- * Membership and association list operations
-- * Equivalence predicates (eq?, eqv?, equal?)
module Primitives.List
    ( -- * Primitive bindings
      listPrimitives
      -- * Basic list operations
    , car
    , cdr
    , cons
    , list'
    , listLength
    , appendLists
    , reverseList
    , listRef
    , listTail
      -- * Membership operations
    , memq
    , memv
    , member
      -- * Association list operations
    , assq
    , assv
    , assoc'
      -- * Equivalence predicates
    , eqv
    , equal
    ) where

import Types
import Control.Monad (zipWithM)
import Control.Monad.Except
import Data.Array (elems)

-- | All list primitive functions
listPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
listPrimitives =
    [ ("car", car)
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
    -- Equivalence predicates
    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    ]

-- | Get the first element of a list
car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- | Get the rest of a list (all but first element)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- | Construct a pair
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xl] = return $ DottedList (x : xs) xl
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- | Create a list from arguments
list' :: [LispVal] -> ThrowsError LispVal
list' = return . List

-- | Get the length of a list
listLength :: [LispVal] -> ThrowsError LispVal
listLength [List xs] = return $ Number $ SInteger $ toInteger $ length xs
listLength [badArg] = throwError $ TypeMismatch "list" badArg
listLength badArgs = throwError $ NumArgs 1 badArgs

-- | Append multiple lists
appendLists :: [LispVal] -> ThrowsError LispVal
appendLists [] = return $ List []
appendLists [x] = return x
appendLists (List x : rest) = do
    result <- appendLists rest
    case result of
        List rest' -> return $ List $ x ++ rest'
        _ -> throwError $ TypeMismatch "list" result
appendLists (badArg : _) = throwError $ TypeMismatch "list" badArg

-- | Reverse a list
reverseList :: [LispVal] -> ThrowsError LispVal
reverseList [List xs] = return $ List $ reverse xs
reverseList [badArg] = throwError $ TypeMismatch "list" badArg
reverseList badArgs = throwError $ NumArgs 1 badArgs

-- | Get element at index
listRef :: [LispVal] -> ThrowsError LispVal
listRef [List xs, Number k] = do
    let idx = round $ schemeNumToDouble k
    if idx < 0 || idx >= toInteger (length xs)
        then throwError $ OutOfRange "list-ref" idx
        else return $ xs !! fromIntegral idx
listRef [badArg, Number _] = throwError $ TypeMismatch "list" badArg
listRef [_, badArg] = throwError $ TypeMismatch "integer" badArg
listRef badArgs = throwError $ NumArgs 2 badArgs

-- | Get tail starting at index
listTail :: [LispVal] -> ThrowsError LispVal
listTail [List xs, Number k] = do
    let idx = round $ schemeNumToDouble k
    if idx < 0 || idx > toInteger (length xs)
        then throwError $ OutOfRange "list-tail" idx
        else return $ List $ drop (fromIntegral idx) xs
listTail [badArg, Number _] = throwError $ TypeMismatch "list" badArg
listTail [_, badArg] = throwError $ TypeMismatch "integer" badArg
listTail badArgs = throwError $ NumArgs 2 badArgs

-- | Find element using eq? comparison
memq :: [LispVal] -> ThrowsError LispVal
memq [_, List []] = return $ Bool False
memq [obj, List (x:xs)] = do
    result <- eqv [obj, x]
    case result of
        Bool True -> return $ List (x:xs)
        _ -> memq [obj, List xs]
memq [_, badArg] = throwError $ TypeMismatch "list" badArg
memq badArgs = throwError $ NumArgs 2 badArgs

-- | Find element using eqv? comparison (same as memq for our implementation)
memv :: [LispVal] -> ThrowsError LispVal
memv = memq

-- | Find element using equal? comparison
member :: [LispVal] -> ThrowsError LispVal
member [_, List []] = return $ Bool False
member [obj, List (x:xs)] = do
    result <- equal [obj, x]
    case result of
        Bool True -> return $ List (x:xs)
        _ -> member [obj, List xs]
member [_, badArg] = throwError $ TypeMismatch "list" badArg
member badArgs = throwError $ NumArgs 2 badArgs

-- | Find association using eq? comparison
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

-- | Find association using eqv? comparison (same as assq)
assv :: [LispVal] -> ThrowsError LispVal
assv = assq

-- | Find association using equal? comparison
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

-- | Check for object identity (eq? and eqv?)
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool b1, Bool b2] = return $ Bool $ b1 == b2
eqv [Number n1, Number n2] = return $ Bool $ n1 == n2
eqv [String s1, String s2] = return $ Bool $ s1 == s2
eqv [Char c1, Char c2] = return $ Bool $ c1 == c2
eqv [Atom a1, Atom a2] = return $ Bool $ a1 == a2
eqv [List [], List []] = return $ Bool True
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- | Check for structural equality (equal?)
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
