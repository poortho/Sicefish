module FENParser where

import Control.Applicative (Alternative (..))

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map

import System.IO
import Prelude hiding (filter)
import Board
import Game
import Pieces

import Data.Void (Void)
import Data.Char (isDigit, ord, digitToInt, toLower, isUpper, intToDigit)

type Parser = Parsec Void String

parseFEN :: Parser GameState
parseFEN = undefined 

piecePlacement :: Int -> Parser Board
piecePlacement row
  | row > 7 = undefined
  | row == 7 = pieceRow row 0
  | otherwise = Map.union <$> pieceRow row 0 <* char '/' <*> piecePlacement (row+1)

pieceRow :: Int -> Int -> Parser Board
pieceRow row col
  | col > 7 = pure Map.empty -- row is fully parsed
  | otherwise = (Map.union <$> pieceTile row col <*> pieceRow row (col+1)) <|> do
      digit <- boundedDigit 1 (8-col)
      pieceRow row (col+digit)

pieceTile :: Int -> Int -> Parser Board
pieceTile i j = Map.singleton (i, j) <$> piece

piece :: Parser Piece
piece = charToPiece <$> (bPiece <|> wPiece)

bPiece :: Parser Char
bPiece = char 'p' <|> char 'n' <|> char 'b' <|> char 'r' <|> char 'q' <|> char 'k'

wPiece :: Parser Char
wPiece = char 'P' <|> char 'N' <|> char 'B' <|> char 'R' <|> char 'Q' <|> char 'K'

canCastle = undefined -- todo when gamestate has castle state lol

activeColor :: Parser Color
activeColor = charToColor <$> (char 'w' <|> char 'b')

enPassant :: Parser (Maybe Index)
enPassant = (Just <$> (frToIndex <$> file <*> eprank)) <|> (const Nothing <$> char '-')

halfMoveClock:: Parser Int
halfMoveClock = L.decimal

fullMoveNumber :: Parser Int
fullMoveNumber = L.decimal

file :: Parser File
file = toEnum <$> (\c -> ord c - ord 'a') <$> satisfy isFile

rank :: Parser Rank
rank = toEnum <$> (\x -> x-1) <$> digitToInt <$> satisfy isRank

eprank :: Parser Rank
eprank = toEnum <$> (\x -> x-1) <$> digitToInt <$> (char '3' <|> char '6')

{- isNonzeroDigit :: Char -> Bool
isNonzeroDigit c = ord c <= ord '9' && ord c >= ord '1' -}

boundedDigit :: Int -> Int -> Parser Int
boundedDigit lower upper = digitToInt <$> satisfy (\c -> isBoundedChar c (intToDigit lower) (intToDigit upper))

isBoundedChar :: Char -> Char -> Char -> Bool
isBoundedChar c lower upper = ord c <= ord upper && ord c >= ord lower

isRank :: Char -> Bool
isRank c = isBoundedChar c '1' '8'

isFile :: Char -> Bool
isFile c = isBoundedChar c 'a' 'h'

charToColor :: Char -> Color
charToColor 'w' = White
charToColor 'b' = Black
charToColor _ = undefined

charToPiece :: Char -> Piece
charToPiece c = case toLower c of
  'p' -> Piece Pawn col
  'n' -> Piece Knight col
  'b' -> Piece Bishop col
  'r' -> Piece Rook col
  'q' -> Piece Queen col
  'k' -> Piece King col
  _ -> undefined
  where col = if isUpper c then White else Black