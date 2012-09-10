module Main where
import System.Environment  
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

main :: IO ()
main = do args <- getArgs
          putStrLn . readExpr $ args !! 0

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString 
        <|> try parseFloat
        <|> try parseRatio
        <|> parseComplex
        <|> parseNumber
        <|> parseCharacter
        <|> parseBool 
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom
               
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x 
                              
parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space") 
                             <|> do {x <- anyChar; notFollowedBy alphaNum; return [x] }
                    return $ Character $ case value of
                      "space" -> ' '
                      "newline" -> '\n'
                      otherwise -> (value !! 0)

parseBool :: Parser LispVal
parseBool = do x <- string "#" >> oneOf "tf" 
               return $ case x of
                 't' -> Bool True
                 'f' -> Bool False

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal)
                  char '+'
                  y <- (try parseFloat <|> parseDecimal)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head $ readFloat (x ++ "." ++ y))
                
parseRatio :: Parser LispVal             
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseNumber :: Parser LispVal
parseNumber = do num <- parseDecimal 
                        <|> parseDecimal' 
                        <|> parseBinary 
                        <|> parseOct 
                        <|> parseHex
                 return $ num

parseDecimal :: Parser LispVal
parseDecimal = do x <- many1 digit
                  (return . Number . read) x

parseDecimal' :: Parser LispVal
parseDecimal' = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x
                   
parseBinary :: Parser LispVal
parseBinary = do try $ string "#b"
                 x <- many1 (oneOf "01")
                 return $ Number (bin2int x)
                 
parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2int x)

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2int x)

bin2int :: String -> Integer
bin2int = bin2int' 0
bin2int' intVal [] = intVal
bin2int' intVal (x:xs) = let old = 2 * intVal + (if x == '0' then 0 else 1) in bin2int' old xs

oct2int x = fst $ readOct x !! 0

hex2int x = fst $ readHex x !! 0

data LispVal = Atom String 
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Character Char
             | Bool Bool    

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do x <- char '\\' >> oneOf "\\\"nrt"
                  return $ case x of 
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _ -> x