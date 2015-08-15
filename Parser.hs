-- TODO: replace all uses of head and tail with pattern matching/total functions
module Parser where
import Text.Parsec hiding (runParser)
import Data.Char (isDigit, isAlpha)
import Control.Monad.Identity

import Ast

type Parser a = ParsecT [Char] () Identity a

parseProgram :: String -> Program
parseProgram = map (either (error . show) id . runParser commandParser) . lines

runParser :: Parser a -> String -> Either ParseError a
runParser parser = parse parser "(source)"

commandParser :: Parser Command
commandParser = do
  command <- many1 letter

  spaces

  rest <- many1 anyChar
  let args = words rest

  return $ case command of
    "print" -> Print $
      case (runParser exprParser rest) of
        Right expr -> expr
        Left  err  -> (error . show) err

    "assign" -> Assign (head . head $ args) $
      case (runParser exprParser (unwords . tail $ args)) of
        Right expr -> expr
        Left  err  -> (error . show) err

    "read" -> Read (head . head $ args)

    _ -> error "Invalid command"

exprParser :: Parser Expr
exprParser = do
  spaces

  itemRaw <- manyTill (digit <|> oneOf ['a'..'z'])
             ((space >> return ()) <|> eof)

  spaces

  rest <- many anyChar
  let item = if (all isDigit itemRaw)
             then Val (read itemRaw)
             else if (isAlpha . head) itemRaw
             then Var (head itemRaw)
             else error "invalid expression"

  return $ case rest of
    ""       -> item
    ('+':xs) -> Add item (parseRest xs)
    ('-':xs) -> Sub item (parseRest xs)
    ('*':xs) -> Mul item (parseRest xs)
    ('/':xs) -> Div item (parseRest xs)
    other    -> error . show $ other
  where
  parseRest :: String -> Expr
  parseRest rest = case (runParser exprParser rest) of
    Right expr -> expr
    Left  err  -> (error . show) err
