module UCIParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Game

data UCICommand = UCI | UCINewGame | IsReady | Position GameState | Go | Quit
  deriving (Eq, Show)

type Parser = Parsec Void String

parseUCICmd :: Parser UCICommand
parseUCICmd = parseSimpleCmd <|> parsePosition -- <|> parseGo

parsePosition :: Parser UCICommand
parsePosition = undefined

parseGo :: Parser UCICommand
parseGo = undefined

parseSimpleCmd :: Parser UCICommand
parseSimpleCmd = strToCmd <$> (string "ucinewgame" <|> string "uci" <|> string "quit" <|> string "isready")

strToCmd :: String -> UCICommand
strToCmd s = case s of
  "uci" -> UCI
  "ucinewgame" -> UCINewGame
  "isready" -> IsReady
  -- "position" -> Position
  -- "go" -> Go
  "quit" -> Quit
  _ -> undefined