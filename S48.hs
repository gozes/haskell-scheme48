module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char hiding (spaces)
import Control.Monad

main = do args <- getArgs
          putStrLn (readExpr (args  !! 0))


symbol:: Parser Char
symbol = oneOf "!$ %&|*+ -/: <=? > @^_ ~# "

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "no macht: lisp " ++ show err
  Right val -> "Found value"

spaces:: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


-- dobleQuates parser = between (char '"') (char '"') parser

-- singleQuates parser = between (char "'") (char "'") parser



r5rstr =  tab <|> endOfLine

-- withQuates :: Parser a -> Parser a
withQuates  = between (char '"') (char '"')

-- parseString :: Parser a ->  Parser LispVal
parseString str = do
  p <- withQuates str
  s <- (many r5rstr)
  return $ String s

-- parseString = do char '"'
--                   x <- many $ parseR5RStr
--                  char '"'
--                  return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit



parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber




