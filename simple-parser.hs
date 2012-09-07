module Main where
import System.Environment  
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = do args <- getArgs
          putStrLn . readExpr $ args !! 0

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber 

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x 

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
-- 練習2.1.1 do記法
--parseNumber = do digits <- many1 digit
--                 return . Number $ read digits
-- 練習2.1.2 >>=演算子を使った明示的なシーケンシング
--parseNumber = many1 digit >>= return . Number . read 

data LispVal = Atom String 
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool    

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space