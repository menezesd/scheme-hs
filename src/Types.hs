{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Types
    ( LispVal(..)
    , LispError(..)
    , ThrowsError
    , IOThrowsError
    , Env
    , nullEnv
    , liftThrows
    , runIOThrows
    , showVal
    , showError
    , SyntaxRules(..)
    , SchemeNum(..)
    , isExact
    , schemeNumToDouble
    , promoteToComplex
    , simplifyNum
    , Continuation(..)
    , SourceInfo(..)
    , noSource
    , showSourceInfo
    , fromSourcePos
    -- Error helpers
    , throwNumArgs
    , throwTypeMismatch
    , throwNotFunction
    , throwUnboundVar
    , throwBadForm
    ) where

import Control.Monad.Except
import Data.IORef
import Text.ParserCombinators.Parsec (ParseError, SourcePos, sourceLine, sourceColumn, sourceName)
import Data.Array (Array)
import Data.Ratio (Ratio, numerator, denominator)
import Data.Complex (Complex(..), realPart, imagPart)
import Data.Map.Strict (Map)
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)

-- | Source location information for error messages
data SourceInfo = SourceInfo
    { sourceFile :: String
    , sourceLine' :: Int
    , sourceCol  :: Int
    } deriving (Eq)

instance Show SourceInfo where
    show = showSourceInfo

-- | Create SourceInfo from Parsec's SourcePos
fromSourcePos :: SourcePos -> SourceInfo
fromSourcePos pos = SourceInfo
    { sourceFile = sourceName pos
    , sourceLine' = sourceLine pos
    , sourceCol = sourceColumn pos
    }

-- | Placeholder for values without source info
noSource :: SourceInfo
noSource = SourceInfo "" 0 0

-- | Display source info
showSourceInfo :: SourceInfo -> String
showSourceInfo si
    | sourceFile si == "" && sourceLine' si == 0 = ""
    | sourceFile si == "" = "line " ++ show (sourceLine' si) ++ ", column " ++ show (sourceCol si)
    | otherwise = sourceFile si ++ ":" ++ show (sourceLine' si) ++ ":" ++ show (sourceCol si)

-- | Scheme numeric tower
-- Hierarchy: Integer < Rational < Real < Complex
-- Each can be exact or inexact
data SchemeNum
    = SInteger Integer              -- Exact integer
    | SRational (Ratio Integer)     -- Exact rational
    | SReal Double                  -- Inexact real
    | SComplex (Complex Double)     -- Complex (can be exact or inexact based on components)
    deriving (Eq)

instance Show SchemeNum where
    show = showSchemeNum

showSchemeNum :: SchemeNum -> String
showSchemeNum (SInteger n) = show n
showSchemeNum (SRational r)
    | denominator r == 1 = show (numerator r)
    | otherwise = show (numerator r) ++ "/" ++ show (denominator r)
showSchemeNum (SReal d)
    | isNaN d = "+nan.0"
    | isInfinite d = if d > 0 then "+inf.0" else "-inf.0"
    | d == fromIntegral (round d :: Integer) = show (round d :: Integer) ++ ".0"
    | otherwise = show d
showSchemeNum (SComplex c)
    | imagPart c == 0 = showSchemeNum (SReal (realPart c))
    | realPart c == 0 = showImag (imagPart c) ++ "i"
    | imagPart c < 0 = showSchemeNum (SReal (realPart c)) ++ showImag (imagPart c) ++ "i"
    | otherwise = showSchemeNum (SReal (realPart c)) ++ "+" ++ showImag (imagPart c) ++ "i"
  where
    showImag i | i == 1 = ""
               | i == -1 = "-"
               | otherwise = show i

-- | Check if a number is exact
isExact :: SchemeNum -> Bool
isExact (SInteger _)  = True
isExact (SRational _) = True
isExact (SReal _)     = False
isExact (SComplex c)  = imagPart c == 0 && realPart c == fromIntegral (round (realPart c) :: Integer)

-- | Convert any scheme number to Double
schemeNumToDouble :: SchemeNum -> Double
schemeNumToDouble (SInteger n)  = fromIntegral n
schemeNumToDouble (SRational r) = fromRational r
schemeNumToDouble (SReal d)     = d
schemeNumToDouble (SComplex c)  = realPart c

-- | Promote to complex
promoteToComplex :: SchemeNum -> Complex Double
promoteToComplex (SInteger n)  = fromIntegral n :+ 0
promoteToComplex (SRational r) = fromRational r :+ 0
promoteToComplex (SReal d)     = d :+ 0
promoteToComplex (SComplex c)  = c

-- | Simplify a number (e.g., complex with 0 imaginary -> real, rational with denom 1 -> integer)
simplifyNum :: SchemeNum -> SchemeNum
simplifyNum (SComplex c)
    | imagPart c == 0 = simplifyNum (SReal (realPart c))
    | otherwise = SComplex c
simplifyNum (SReal d)
    | isNaN d || isInfinite d = SReal d
    | d == fromIntegral (round d :: Integer) = SInteger (round d)
    | otherwise = SReal d
simplifyNum (SRational r)
    | denominator r == 1 = SInteger (numerator r)
    | otherwise = SRational r
simplifyNum n = n

-- | Continuation type for call/cc
newtype Continuation = Continuation (LispVal -> IOThrowsError LispVal)

-- | The core Scheme value type
data LispVal
    = Atom String                          -- Symbols/identifiers
    | List [LispVal]                       -- Proper lists (immutable)
    | DottedList [LispVal] LispVal         -- Improper lists (a b . c)
    | MutablePair (IORef LispVal) (IORef LispVal)  -- Mutable cons cell for set-car!/set-cdr!
    | Number SchemeNum                     -- Full numeric tower
    | String String                        -- Strings
    | Char Char                            -- Characters
    | Bool Bool                            -- Booleans
    | Vector (Array Int LispVal)           -- Vectors
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle                          -- I/O ports
    | EOF                                  -- End-of-file object
    | Func { params  :: [String]
           , vararg  :: Maybe String
           , body    :: [LispVal]
           , closure :: Env
           }
    | Macro { macroParams  :: [String]
            , macroVararg  :: Maybe String
            , macroBody    :: [LispVal]
            }
    | Syntax SyntaxRules                   -- R5RS syntax-rules macro
    | Cont Continuation                    -- First-class continuation
    | Void                                 -- Unspecified value

-- | Syntax-rules representation
data SyntaxRules = SyntaxRules
    { syntaxLiterals :: [String]
    , syntaxRules    :: [(LispVal, LispVal)]  -- (pattern, template) pairs
    }

instance Show LispVal where
    show = showVal

instance Eq LispVal where
    (Atom a1) == (Atom a2)         = a1 == a2
    (List l1) == (List l2)         = l1 == l2
    (DottedList h1 t1) == (DottedList h2 t2) = h1 == h2 && t1 == t2
    (Number n1) == (Number n2)     = n1 == n2
    (String s1) == (String s2)     = s1 == s2
    (Char c1) == (Char c2)         = c1 == c2
    (Bool b1) == (Bool b2)         = b1 == b2
    EOF == EOF                     = True
    Void == Void                   = True
    _ == _                         = False

showVal :: LispVal -> String
showVal (Atom name)       = name
showVal (List contents)   = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t)  = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (MutablePair carRef cdrRef) =
    -- Use unsafePerformIO for display (safe here as it's read-only)
    let car' = unsafePerformIO $ readIORef carRef
        cdr' = unsafePerformIO $ readIORef cdrRef
    in case cdr' of
        List [] -> "(" ++ showVal car' ++ ")"
        List xs -> "(" ++ showVal car' ++ " " ++ unwordsList xs ++ ")"
        _       -> "(" ++ showVal car' ++ " . " ++ showVal cdr' ++ ")"
showVal (Number n)        = show n
showVal (String s)        = "\"" ++ escapeString s ++ "\""
showVal (Char c)          = showChar' c
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (Vector arr)      = "#(" ++ unwordsList (elems arr) ++ ")"
  where elems = foldr (:) []
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _)        = "<IO primitive>"
showVal (Port _)          = "<IO port>"
showVal EOF               = "#<eof>"
showVal (Cont _)          = "<continuation>"
showVal (Func args vararg' _ _) =
    "(lambda (" ++ unwords args ++
    (case vararg' of
        Nothing  -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Macro args vararg' _) =
    "(macro (" ++ unwords args ++
    (case vararg' of
        Nothing  -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Syntax _)        = "<syntax>"
showVal Void              = ""

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c    = [c]

showChar' :: Char -> String
showChar' ' '  = "#\\space"
showChar' '\n' = "#\\newline"
showChar' '\t' = "#\\tab"
showChar' c    = "#\\" ++ [c]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | Error types with optional source location
data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | DivideByZero
    | OutOfRange String Integer
    | Default String
    | ContinuationJump LispVal
    | WithSource SourceInfo LispError  -- Wrapper for errors with source location

showError :: LispError -> String
showError (WithSource src err) =
    let srcStr = showSourceInfo src
    in if null srcStr
        then showError err
        else srcStr ++ ": " ++ showError err
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError DivideByZero = "Division by zero"
showError (OutOfRange message idx) = message ++ ": " ++ show idx
showError (Default message) = message
showError (ContinuationJump _) = "Continuation called outside of call/cc"

instance Show LispError where
    show = showError

-- | Error helper functions for cleaner error throwing
throwNumArgs :: MonadError LispError m => Integer -> [LispVal] -> m a
throwNumArgs n args = throwError $ NumArgs n args

throwTypeMismatch :: MonadError LispError m => String -> LispVal -> m a
throwTypeMismatch expected found = throwError $ TypeMismatch expected found

throwNotFunction :: MonadError LispError m => String -> m a
throwNotFunction name = throwError $ NotFunction "Not a function" name

throwUnboundVar :: MonadError LispError m => String -> m a
throwUnboundVar var = throwError $ UnboundVar "Unbound variable" var

throwBadForm :: MonadError LispError m => String -> LispVal -> m a
throwBadForm msg form = throwError $ BadSpecialForm msg form

type ThrowsError = Either LispError

-- | Environment: mutable bindings using Map for O(log n) lookup
type Env = IORef (Map String (IORef LispVal))

nullEnv :: IO Env
nullEnv = newIORef mempty

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
    result <- runExceptT action
    return $ case result of
        Left err  -> show err
        Right val -> val
