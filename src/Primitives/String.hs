{-# LANGUAGE FlexibleContexts #-}

-- | String primitives for R5RS Scheme
--
-- This module implements string operations including:
--
-- * String accessors and constructors
-- * String comparison (case-sensitive and case-insensitive)
-- * String to/from list conversion
-- * String mutation (returns new strings, as Haskell strings are immutable)
-- * Number/string conversion
module Primitives.String
    ( -- * Primitive bindings
      stringPrimitives
      -- * String operations
    , stringLength
    , stringRef
    , stringAppend
    , substring
    , makeString
    , stringFromChars
    , stringCopy
    , stringSet
    , stringFill
      -- * String comparison
    , strBoolBinop
    , strCiBoolBinop
      -- * Conversions
    , stringToList
    , listToString
    , numberToString
    , stringToNumber
    ) where

import Types
import Parser (readExpr)
import Control.Monad.Except
import Data.Char (toLower)

-- | All string primitive functions
stringPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
stringPrimitives =
    [ ("string-length", unaryOp stringLength)
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
    , ("string-set!", stringSet)
    , ("string-fill!", stringFill)
    ]

-- | Apply a unary function to a single argument
unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ args = throwError $ NumArgs 1 args

-- | Get the length of a string
stringLength :: LispVal -> ThrowsError LispVal
stringLength (String s) = return $ Number $ SInteger $ toInteger $ length s
stringLength notStr = throwError $ TypeMismatch "string" notStr

-- | Get a character from a string by index
stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number k] = do
    let idx = round $ schemeNumToDouble k
    if idx < 0 || idx >= toInteger (length s)
        then throwError $ OutOfRange "string-ref" idx
        else return $ Char $ s !! fromIntegral idx
stringRef [String _, notNum] = throwError $ TypeMismatch "integer" notNum
stringRef [notStr, _] = throwError $ TypeMismatch "string" notStr
stringRef args = throwError $ NumArgs 2 args

-- | Concatenate strings
stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend args = do
    strs <- mapM unpackStr args
    return $ String $ concat strs
  where
    unpackStr (String s) = return s
    unpackStr notStr = throwError $ TypeMismatch "string" notStr

-- | Extract a substring
substring :: [LispVal] -> ThrowsError LispVal
substring [String s, Number start', Number end'] = do
    let start = round $ schemeNumToDouble start'
    let end = round $ schemeNumToDouble end'
    if start < 0 || end > toInteger (length s) || start > end
        then throwError $ Default "substring: invalid indices"
        else return $ String $ take (fromIntegral $ end - start) $ drop (fromIntegral start) s
substring args = throwError $ NumArgs 3 args

-- | Case-sensitive string comparison
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop op [String s1, String s2] = return $ Bool $ op s1 s2
strBoolBinop _ [String _, notStr] = throwError $ TypeMismatch "string" notStr
strBoolBinop _ [notStr, _] = throwError $ TypeMismatch "string" notStr
strBoolBinop _ args = throwError $ NumArgs 2 args

-- | Case-insensitive string comparison
strCiBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strCiBoolBinop op [String s1, String s2] = return $ Bool $ op (map toLower s1) (map toLower s2)
strCiBoolBinop _ [String _, notStr] = throwError $ TypeMismatch "string" notStr
strCiBoolBinop _ [notStr, _] = throwError $ TypeMismatch "string" notStr
strCiBoolBinop _ args = throwError $ NumArgs 2 args

-- | Convert string to list of characters
stringToList :: LispVal -> ThrowsError LispVal
stringToList (String s) = return $ List $ map Char s
stringToList notStr = throwError $ TypeMismatch "string" notStr

-- | Convert list of characters to string
listToString :: LispVal -> ThrowsError LispVal
listToString (List cs) = do
    chars <- mapM unpackChar cs
    return $ String chars
  where
    unpackChar (Char c) = return c
    unpackChar notChar = throwError $ TypeMismatch "char" notChar
listToString notList = throwError $ TypeMismatch "list" notList

-- | Create a string of given length
makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number k] = return $ String $ replicate (fromIntegral $ round $ schemeNumToDouble k) ' '
makeString [Number k, Char c] = return $ String $ replicate (fromIntegral $ round $ schemeNumToDouble k) c
makeString args = throwError $ NumArgs 1 args

-- | Create a string from characters
stringFromChars :: [LispVal] -> ThrowsError LispVal
stringFromChars args = do
    chars <- mapM unpackChar args
    return $ String chars
  where
    unpackChar (Char c) = return c
    unpackChar notChar = throwError $ TypeMismatch "char" notChar

-- | Copy a string (identity in Haskell)
stringCopy :: LispVal -> ThrowsError LispVal
stringCopy (String s) = return $ String s
stringCopy notStr = throwError $ TypeMismatch "string" notStr

-- | Convert number to string (with optional radix)
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

-- | Convert string to number (with optional radix)
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

-- | Set a character in a string (returns new string)
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

-- | Fill a string with a character (returns new string)
stringFill :: [LispVal] -> ThrowsError LispVal
stringFill [String s, Char c] = return $ String $ replicate (length s) c
stringFill [String _, badArg] = throwError $ TypeMismatch "char" badArg
stringFill [badArg, _] = throwError $ TypeMismatch "string" badArg
stringFill badArgs = throwError $ NumArgs 2 badArgs
