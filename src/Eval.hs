module Eval where

import Pieces
import Game

evalPosition :: GameState -> Int
evalPosition = materialDiff

materialDiff :: GameState -> Int
materialDiff (GameState board _ _ _ _ _ _ _ _) = foldr f 0 board
  where f (Piece pt White) acc = acc + pieceToMaterial pt
        f (Piece pt Black) acc = acc - pieceToMaterial pt

pieceToMaterial :: PieceType -> Int
pieceToMaterial pt = case pt of
  Pawn -> 1
  Knight -> 3
  Bishop -> 3 -- maybe increase this to like 3.1 lol
  Rook -> 5
  Queen -> 9
  King -> 0 -- only 1 king and both sides always have it
