module Parser
    ( readExpr
    , readExprList
    ) where

import Types
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Array (listArray)
import Data.Char (digitToInt)
import Data.Ratio ((%))
import Data.Complex (Complex(..), realPart, imagPart)
import Numeric (readHex, readOct)

-- | Parse a single expression
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> Left $ Parser err
    Right val -> Right val

-- | Parse multiple expressions
readExprList :: String -> ThrowsError [LispVal]
readExprList input = case parse (spaces >> many parseExpr) "lisp" input of
    Left err  -> Left $ Parser err
    Right val -> Right val

-- | Whitespace and comments
spaces :: Parser ()
spaces = skipMany (space' <|> comment)
  where
    space' = oneOf " \t\n\r" >> return ()
    comment = char ';' >> skipMany (noneOf "\n\r")

-- | Parse any expression
parseExpr :: Parser LispVal
parseExpr = do
    spaces
    expr <- try parseNumber
        <|> parseString
        <|> try parseChar
        <|> try parseBool
        <|> parseAtom
        <|> parseQuoted
        <|> parseQuasiquoted
        <|> parseUnquote
        <|> parseUnquoteSplicing
        <|> parseVector
        <|> parseList
    spaces
    return expr

-- | Parse an atom (symbol)
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ Atom atom

-- | Valid symbol characters
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- | Parse a string literal
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (escapedChar <|> noneOf "\"\\")
    _ <- char '"'
    return $ String x

escapedChar :: Parser Char
escapedChar = do
    _ <- char '\\'
    c <- oneOf "\\\"nrt"
    return $ case c of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _   -> c

-- | Parse a character literal
parseChar :: Parser LispVal
parseChar = do
    _ <- try $ string "#\\"
    c <- parseCharName <|> anyChar
    return $ Char c

parseCharName :: Parser Char
parseCharName = try (string "space" >> return ' ')
            <|> try (string "newline" >> return '\n')
            <|> try (string "tab" >> return '\t')

-- | Parse a boolean
parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    c <- oneOf "tfTF"
    return $ Bool (c `elem` "tT")

-- | Parse a number (integer, float, rational, complex, hex, octal, binary)
parseNumber :: Parser LispVal
parseNumber = parseRadixNumber <|> parseDecimalNumber

parseRadixNumber :: Parser LispVal
parseRadixNumber = do
    _ <- char '#'
    exactness <- optionMaybe (oneOf "eEiI")
    radix <- optionMaybe (oneOf "xXoObBdD")
    case radix of
        Just r | r `elem` "xX" -> parseHex exactness
               | r `elem` "oO" -> parseOct exactness
               | r `elem` "bB" -> parseBin exactness
               | otherwise     -> parseDecimalNumber' exactness
        Nothing -> case exactness of
            Just _ -> parseDecimalNumber' exactness
            Nothing -> fail "Unknown number prefix"

parseHex :: Maybe Char -> Parser LispVal
parseHex exact = do
    sign <- optionMaybe (oneOf "+-")
    digits <- many1 hexDigit
    case readHex digits of
        [(n, "")] -> return $ Number $ applyExactness exact $ applySign sign $ SInteger n
        _ -> fail "Invalid hex number"

parseOct :: Maybe Char -> Parser LispVal
parseOct exact = do
    sign <- optionMaybe (oneOf "+-")
    digits <- many1 octDigit
    case readOct digits of
        [(n, "")] -> return $ Number $ applyExactness exact $ applySign sign $ SInteger n
        _ -> fail "Invalid octal number"

parseBin :: Maybe Char -> Parser LispVal
parseBin exact = do
    sign <- optionMaybe (oneOf "+-")
    digits <- many1 (oneOf "01")
    let n = foldl (\acc d -> acc * 2 + toInteger (digitToInt d)) 0 digits
    return $ Number $ applyExactness exact $ applySign sign $ SInteger n

applySign :: Maybe Char -> SchemeNum -> SchemeNum
applySign (Just '-') (SInteger n)  = SInteger (-n)
applySign (Just '-') (SRational r) = SRational (-r)
applySign (Just '-') (SReal d)     = SReal (-d)
applySign (Just '-') (SComplex c)  = SComplex (-c)
applySign _ n = n

applyExactness :: Maybe Char -> SchemeNum -> SchemeNum
applyExactness (Just c) n
    | c `elem` "iI" = toInexact n
    | c `elem` "eE" = toExact n
applyExactness _ n = n

toInexact :: SchemeNum -> SchemeNum
toInexact (SInteger n)  = SReal (fromIntegral n)
toInexact (SRational r) = SReal (fromRational r)
toInexact n = n

toExact :: SchemeNum -> SchemeNum
toExact (SReal d)
    | d == fromIntegral (round d :: Integer) = SInteger (round d)
    | otherwise = SRational (toRational d)
toExact (SComplex c)
    | imagPart c == 0 = toExact (SReal (realPart c))
    | otherwise = SComplex c  -- Can't make complex exact easily
toExact n = n

parseDecimalNumber :: Parser LispVal
parseDecimalNumber = parseDecimalNumber' Nothing

parseDecimalNumber' :: Maybe Char -> Parser LispVal
parseDecimalNumber' exact = try parseComplex
                        <|> try parseRational
                        <|> try parseFloat'
                        <|> parseInt' exact

parseComplex :: Parser LispVal
parseComplex = do
    real <- parseRealPart
    sign <- oneOf "+-"
    imag <- parseImagPart
    _ <- char 'i'
    let imagVal = if sign == '-' then -imag else imag
    return $ Number $ SComplex (real :+ imagVal)

parseRealPart :: Parser Double
parseRealPart = do
    sign <- optionMaybe (char '-')
    num <- parseUnsignedReal
    return $ case sign of
        Just _ -> -num
        Nothing -> num

parseImagPart :: Parser Double
parseImagPart = parseUnsignedReal <|> return 1.0

parseUnsignedReal :: Parser Double
parseUnsignedReal = try parseFloatValue <|> parseIntValue

parseFloatValue :: Parser Double
parseFloatValue = do
    whole <- many digit
    _ <- char '.'
    frac <- many1 digit
    expo <- optionMaybe parseExponent
    let numStr = (if null whole then "0" else whole) ++ "." ++ frac
    let baseNum = read numStr :: Double
    return $ case expo of
        Nothing -> baseNum
        Just e  -> baseNum * (10 ** fromIntegral e)

parseIntValue :: Parser Double
parseIntValue = do
    digits <- many1 digit
    return $ read digits

parseRational :: Parser LispVal
parseRational = do
    sign <- optionMaybe (oneOf "+-")
    num <- many1 digit
    _ <- char '/'
    denom <- many1 digit
    let n = read num :: Integer
    let d = read denom :: Integer
    if d == 0
        then fail "Division by zero in rational"
        else return $ Number $ applySign sign $ simplifyNum $ SRational (n % d)

parseFloat' :: Parser LispVal
parseFloat' = do
    sign <- optionMaybe (oneOf "+-")
    whole <- many digit
    _ <- char '.'
    frac <- many1 digit
    expo <- optionMaybe parseExponent
    let numStr = (if null whole then "0" else whole) ++ "." ++ frac
    let baseNum = read numStr :: Double
    let withExp = case expo of
            Nothing -> baseNum
            Just e  -> baseNum * (10 ** fromIntegral e)
    return $ Number $ applySign sign $ SReal withExp

parseInt' :: Maybe Char -> Parser LispVal
parseInt' exact = do
    sign <- optionMaybe (oneOf "+-")
    digits <- many1 digit
    let n = read digits :: Integer
    return $ Number $ applyExactness exact $ applySign sign $ SInteger n

parseExponent :: Parser Integer
parseExponent = do
    _ <- oneOf "eE"
    sign <- optionMaybe (oneOf "+-")
    digits <- many1 digit
    let n = read digits
    return $ case sign of
        Just '-' -> negate n
        _        -> n

-- | Parse quoted expression
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- | Parse quasiquoted expression
parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
    _ <- char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

-- | Parse unquote
parseUnquote :: Parser LispVal
parseUnquote = do
    _ <- try $ char ',' >> notFollowedBy (char '@')
    x <- parseExpr
    return $ List [Atom "unquote", x]

-- | Parse unquote-splicing
parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    _ <- try $ string ",@"
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

-- | Parse a vector
parseVector :: Parser LispVal
parseVector = do
    _ <- try $ string "#("
    spaces
    elems <- sepEndBy parseExpr spaces
    _ <- char ')'
    return $ Vector $ listArray (0, length elems - 1) elems

-- | Parse a list (proper or dotted)
parseList :: Parser LispVal
parseList = do
    _ <- char '('
    spaces
    parseListContents

parseListContents :: Parser LispVal
parseListContents = do
    elems <- sepEndBy parseExpr spaces
    dotted <- optionMaybe (char '.' >> spaces >> parseExpr)
    spaces
    _ <- char ')'
    return $ case dotted of
        Nothing   -> List elems
        Just tail' -> DottedList elems tail'
