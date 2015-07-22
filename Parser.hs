-- TODO: replace all uses of head and tail with pattern matching/total functions
module Parser where
import qualified Text.Parsec as Parsec
import Data.Char (isDigit)

import Ast

parseProgram :: String -> Program
parseProgram = (map ((either (error . show) id) . (runParser commandParser))) . lines

-- TODO: Add type signature
runParser parser text = Parsec.parse parser "(source)" text

commandParser :: Parsec.Parsec String () Command
commandParser = do
  command <- Parsec.many1 Parsec.letter
  Parsec.spaces
  rest <- Parsec.many1 Parsec.anyChar
  let args = words rest
  return $ case command of
    "print" -> Print $ case (runParser exprParser rest) of
      Right expr -> expr
      Left err -> (error . show) err
    "assign" -> Assign (head . head $ args) $ case (runParser exprParser (unwords . tail $ args)) of
      Right expr -> expr
      Left err -> (error . show) err
    "read" -> Read (head . head $ args)
    _ -> error "Invalid command"

exprParser :: Parsec.Parsec String () Expr
exprParser = do
  Parsec.spaces

  itemRaw <- Parsec.manyTill (Parsec.digit Parsec.<|> Parsec.oneOf ['a'..'z']) ((Parsec.space >> return ()) Parsec.<|> Parsec.eof)
  Parsec.spaces

  rest <- Parsec.many Parsec.anyChar
  let item = if (all isDigit itemRaw) then Val (read itemRaw) else Var (head itemRaw)

  return $ case rest of
    "" -> item
    ('+':xs) -> Add item $ case (runParser exprParser xs) of
      Right expr -> expr
      Left err -> (error . show) err
    ('-':xs) -> Sub item $ case (runParser exprParser xs) of
      Right expr -> expr
      Left err -> (error . show) err
    ('*':xs) -> Mul item $ case (runParser exprParser xs) of
      Right expr -> expr
      Left err -> (error . show) err
    ('/':xs) -> Div item $ case (runParser exprParser xs) of
      Right expr -> expr
      Left err -> (error . show) err
    other -> error . show $ other
