module UCIParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Game
import Move
import FENParser
import Data.Maybe
import Board
import Pieces

data UCICommand = UCI | UCINewGame | IsReady | Position GameState | Go | Quit
  deriving (Eq, Show)

parseUCICmd :: Parser UCICommand
parseUCICmd = parseSimpleCmd <|> parsePosition -- <|> parseGo

parsePosition :: Parser UCICommand
parsePosition = Position <$> fromJust <$> (playMoves <$> (Just <$> (string "position" *> space *> (parseFEN <|> (const startState <$> string "startpos")))) <*>
  (unwrapMaybeList <$> optional (space *> string "moves" *> space *> sepBy1 parseMove space)))

parseMove :: Parser Move
parseMove = parseToMove <$> (frToIndex <$> file <*> rank) <*>  (frToIndex <$> file <*> rank) <*> promoChar

parseToMove :: Index -> Index -> Maybe Char -> Move
parseToMove src dst c = Move src dst (if c == Nothing then Quiet else Promotion) (maybeToPromo c)

maybeToPromo :: Maybe Char -> Maybe PieceType
maybeToPromo Nothing = Nothing
maybeToPromo (Just c) = case c of
  'q' -> Just Queen
  'b' -> Just Bishop
  'n' -> Just Knight
  'r' -> Just Rook
  _ -> undefined

promoChar :: Parser (Maybe Char)
promoChar = optional (char 'b' <|> char 'n' <|> char 'r' <|> char 'q')

-- is there a better way to do this lmao
unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList Nothing = []
unwrapMaybeList (Just l) = l

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