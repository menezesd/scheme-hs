{-# LANGUAGE FlexibleContexts #-}

-- | I/O primitives for R5RS Scheme
--
-- This module implements I/O operations including:
--
-- * Display, write, and read procedures
-- * Port operations (open, close, current ports)
-- * Character I/O
-- * File operations
-- * Mutable pair operations
-- * Environment procedures
module Primitives.IO
    ( -- * Primitive bindings
      ioPrimitives
      -- * Display and output
    , displayProc
    , newlineProc
    , writeProc
    , showValForDisplay
      -- * Input
    , readProc
    , load
      -- * Port predicates
    , isPort
    , isInputPort
    , isOutputPort
    , isEOFObject
      -- * Port operations
    , openInputFile
    , openOutputFile
    , closePort
    , currentInputPort
    , currentOutputPort
      -- * Character I/O
    , readChar
    , peekChar
    , writeChar
    , charReady
      -- * File operations
    , callWithInputFile
    , callWithOutputFile
    , withInputFromFile
    , withOutputToFile
      -- * Mutable pairs
    , mcons
    , mcar
    , mcdr
    , mpairQ
    , setCar
    , setCdr
      -- * Environment procedures
    , schemeReportEnv
    , nullEnvProc
    , interactionEnv
      -- * Multiple values
    , callWithValues
      -- * Transcript (no-ops)
    , transcriptOn
    , transcriptOff
    ) where

import Types
import Parser (readExpr, readExprList)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO
import Control.Exception (try, IOException)

-- | All I/O primitive functions
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
    , ("mcons", mcons)
    , ("mcar", mcar)
    , ("mcdr", mcdr)
    , ("mpair?", mpairQ)
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

-- | Display a value (without quotes for strings)
displayProc :: [LispVal] -> IOThrowsError LispVal
displayProc [val] = displayProc [val, Port stdout]
displayProc [String s, Port h] = liftIO (hPutStr h s) >> return Void
displayProc [val, Port h] = liftIO (hPutStr h $ showValForDisplay val) >> return Void
displayProc [_, badArg] = throwError $ TypeMismatch "port" badArg
displayProc badArgs = throwError $ NumArgs 1 badArgs

-- | Format a value for display (no quotes on strings)
showValForDisplay :: LispVal -> String
showValForDisplay (String s) = s
showValForDisplay v = show v

-- | Output a newline
newlineProc :: [LispVal] -> IOThrowsError LispVal
newlineProc [] = newlineProc [Port stdout]
newlineProc [Port h] = liftIO (hPutStr h "\n") >> return Void
newlineProc [badArg] = throwError $ TypeMismatch "port" badArg
newlineProc badArgs = throwError $ NumArgs 0 badArgs

-- | Write a value (with quotes for strings)
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [val] = writeProc [val, Port stdout]
writeProc [val, Port h] = liftIO (hPutStr h $ show val) >> return Void
writeProc [_, badArg] = throwError $ TypeMismatch "port" badArg
writeProc badArgs = throwError $ NumArgs 1 badArgs

-- | Read a Scheme expression
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

-- | Load and parse a Scheme file
load :: [LispVal] -> IOThrowsError LispVal
load [String filename] = do
    contents <- liftIO $ readFile filename
    liftThrows $ readExprList contents >>= return . List
load [badArg] = throwError $ TypeMismatch "string" badArg
load badArgs = throwError $ NumArgs 1 badArgs

-- | Check if value is a port
isPort :: LispVal -> ThrowsError LispVal
isPort (Port _) = return $ Bool True
isPort _ = return $ Bool False

-- | Check if value is an input port
isInputPort :: LispVal -> ThrowsError LispVal
isInputPort (Port _) = return $ Bool True
isInputPort _ = return $ Bool False

-- | Check if value is an output port
isOutputPort :: LispVal -> ThrowsError LispVal
isOutputPort (Port _) = return $ Bool True
isOutputPort _ = return $ Bool False

-- | Check if value is the EOF object
isEOFObject :: LispVal -> ThrowsError LispVal
isEOFObject EOF = return $ Bool True
isEOFObject _ = return $ Bool False

-- | Open a file for reading
openInputFile :: [LispVal] -> IOThrowsError LispVal
openInputFile [String filename] = do
    result <- liftIO $ try (openFile filename ReadMode)
    case result of
        Left e -> throwError $ Default $ "Cannot open file: " ++ show (e :: IOException)
        Right h -> return $ Port h
openInputFile [badArg] = throwError $ TypeMismatch "string" badArg
openInputFile badArgs = throwError $ NumArgs 1 badArgs

-- | Open a file for writing
openOutputFile :: [LispVal] -> IOThrowsError LispVal
openOutputFile [String filename] = do
    result <- liftIO $ try (openFile filename WriteMode)
    case result of
        Left e -> throwError $ Default $ "Cannot open file: " ++ show (e :: IOException)
        Right h -> return $ Port h
openOutputFile [badArg] = throwError $ TypeMismatch "string" badArg
openOutputFile badArgs = throwError $ NumArgs 1 badArgs

-- | Close a port
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port h] = liftIO (hClose h) >> return Void
closePort [badArg] = throwError $ TypeMismatch "port" badArg
closePort badArgs = throwError $ NumArgs 1 badArgs

-- | Get the current input port (stdin)
currentInputPort :: [LispVal] -> IOThrowsError LispVal
currentInputPort [] = return $ Port stdin
currentInputPort badArgs = throwError $ NumArgs 0 badArgs

-- | Get the current output port (stdout)
currentOutputPort :: [LispVal] -> IOThrowsError LispVal
currentOutputPort [] = return $ Port stdout
currentOutputPort badArgs = throwError $ NumArgs 0 badArgs

-- | Read a single character
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

-- | Peek at the next character without consuming it
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

-- | Write a single character
writeChar :: [LispVal] -> IOThrowsError LispVal
writeChar [Char c] = writeChar [Char c, Port stdout]
writeChar [Char c, Port h] = liftIO (hPutChar h c) >> return Void
writeChar [Char _, badArg] = throwError $ TypeMismatch "port" badArg
writeChar [badArg, _] = throwError $ TypeMismatch "char" badArg
writeChar badArgs = throwError $ NumArgs 1 badArgs

-- | Check if a character is ready to be read
charReady :: [LispVal] -> IOThrowsError LispVal
charReady [] = charReady [Port stdin]
charReady [Port h] = do
    ready <- liftIO $ hReady h
    return $ Bool ready
charReady [badArg] = throwError $ TypeMismatch "port" badArg
charReady badArgs = throwError $ NumArgs 1 badArgs

-- | Call a procedure with an input file (requires eval support)
callWithInputFile :: [LispVal] -> IOThrowsError LispVal
callWithInputFile [String _, _] =
    throwError $ Default "call-with-input-file: requires apply (implement in Eval)"
callWithInputFile badArgs = throwError $ NumArgs 2 badArgs

-- | Call a procedure with an output file (requires eval support)
callWithOutputFile :: [LispVal] -> IOThrowsError LispVal
callWithOutputFile [String _, _] =
    throwError $ Default "call-with-output-file: requires apply (implement in Eval)"
callWithOutputFile badArgs = throwError $ NumArgs 2 badArgs

-- | Redirect input from a file (requires eval support)
withInputFromFile :: [LispVal] -> IOThrowsError LispVal
withInputFromFile [String _, _] =
    throwError $ Default "with-input-from-file: requires apply (implement in Eval)"
withInputFromFile badArgs = throwError $ NumArgs 2 badArgs

-- | Redirect output to a file (requires eval support)
withOutputToFile :: [LispVal] -> IOThrowsError LispVal
withOutputToFile [String _, _] =
    throwError $ Default "with-output-to-file: requires apply (implement in Eval)"
withOutputToFile badArgs = throwError $ NumArgs 2 badArgs

-- | Create a mutable pair
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

-- | Set the car of a pair
setCar :: [LispVal] -> IOThrowsError LispVal
setCar [MutablePair carRef _, val] = do
    liftIO $ writeIORef carRef val
    return Void
setCar [List (_:xs), val] = return $ List (val:xs)
setCar [DottedList (_:xs) t, val] = return $ DottedList (val:xs) t
setCar [List [], _] = throwError $ TypeMismatch "pair" (List [])
setCar [badArg, _] = throwError $ TypeMismatch "pair" badArg
setCar badArgs = throwError $ NumArgs 2 badArgs

-- | Set the cdr of a pair
setCdr :: [LispVal] -> IOThrowsError LispVal
setCdr [MutablePair _ cdrRef, val] = do
    liftIO $ writeIORef cdrRef val
    return Void
setCdr [List (x:_), List ys] = return $ List (x:ys)
setCdr [List (x:_), val] = return $ DottedList [x] val
setCdr [DottedList (x:_) _, List ys] = return $ List (x:ys)
setCdr [DottedList (x:_) _, val] = return $ DottedList [x] val
setCdr [List [], _] = throwError $ TypeMismatch "pair" (List [])
setCdr [badArg, _] = throwError $ TypeMismatch "pair" badArg
setCdr badArgs = throwError $ NumArgs 2 badArgs

-- | Get R5RS scheme-report-environment
schemeReportEnv :: [LispVal] -> IOThrowsError LispVal
schemeReportEnv [Number _] = do
    return $ List [Atom "scheme-report-environment", Number (SInteger 5)]
schemeReportEnv [badArg] = throwError $ TypeMismatch "integer" badArg
schemeReportEnv badArgs = throwError $ NumArgs 1 badArgs

-- | Get null environment (only special forms)
nullEnvProc :: [LispVal] -> IOThrowsError LispVal
nullEnvProc [Number _] = do
    return $ List [Atom "null-environment", Number (SInteger 5)]
nullEnvProc [badArg] = throwError $ TypeMismatch "integer" badArg
nullEnvProc badArgs = throwError $ NumArgs 1 badArgs

-- | Get interaction environment
interactionEnv :: [LispVal] -> IOThrowsError LispVal
interactionEnv [] = return $ List [Atom "interaction-environment"]
interactionEnv badArgs = throwError $ NumArgs 0 badArgs

-- | Call with values (requires eval support)
callWithValues :: [LispVal] -> IOThrowsError LispVal
callWithValues [_, _] =
    throwError $ Default "call-with-values: requires apply (implement in Eval)"
callWithValues badArgs = throwError $ NumArgs 2 badArgs

-- | Enable transcript (no-op)
transcriptOn :: [LispVal] -> IOThrowsError LispVal
transcriptOn [String _] = return Void
transcriptOn badArgs = throwError $ NumArgs 1 badArgs

-- | Disable transcript (no-op)
transcriptOff :: [LispVal] -> IOThrowsError LispVal
transcriptOff [] = return Void
transcriptOff badArgs = throwError $ NumArgs 0 badArgs
