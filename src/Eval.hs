module Eval where

import Pieces
import Game
import Board
import qualified Data.Map as Map

evalPosition :: GameState -> Int
evalPosition s = materialDiff s + pieceSquare s

materialDiff :: GameState -> Int
materialDiff (GameState board _ _ _ _ _ _ _ _) = foldr f 0 board
  where f (Piece pt White) acc = acc + pieceToMaterial pt
        f (Piece pt Black) acc = acc - pieceToMaterial pt

pieceToMaterial :: PieceType -> Int
pieceToMaterial pt = case pt of
  Pawn -> 100
  Knight -> 300
  Bishop -> 310 -- maybe increase this to like 3.1 lol
  Rook -> 500
  Queen -> 900
  King -> 0 -- only 1 king and both sides always have it

pieceSquare :: GameState -> Int
pieceSquare (GameState board _ _ _ _ _ _ _ _) = Map.foldrWithKey (\k v a -> a + pieceSquareCalc k v) 0 board

pieceSquareCalc :: Index -> Piece -> Int
pieceSquareCalc (i, j) (Piece pt col) = case pt of
  Knight -> g $ (f pieceSquareKnight) !! j !! i
  Pawn -> g $ (f pieceSquarePawn) !! j !! i
  Bishop -> g $ (f pieceSquareBishop) !! j !! i
  Rook -> g $ (f pieceSquareBishop) !! j !! i
  _ -> 0
  where f = if col == White then id else reverse
        g = if col == White then id else negate

pieceSquarePawn :: [[Int]]
pieceSquarePawn = reverse [ -- reverse it coz (0,0) is a1 in our code
  [0,   0,   0,   0,   0,   0,   0,   0],
  [5,  10,  15,  20,  20,  15,  10,   5],
  [4,   8,  12,  16,  16,  12,   8,   4],
  [3,   6,   9,  12,  12,   9,   6,   3],
  [2,   4,   6,   8,   8,   6,   4,   2],
  [1,   2,   3, -10, -10,   3,   2,   1],
  [0,   0,   0, -40, -40,   0,   0,   0],
  [0,   0,   0,   0,   0,   0,   0,   0]]

pieceSquareKnight :: [[Int]]
pieceSquareKnight = [ -- symmetric, no need to reverse
  [-10, -10, -10, -10, -10, -10, -10, -10],
  [-10,   0,   0,   0,   0,   0,   0, -10],
  [-10,   0,   5,   5,   5,   5,   0, -10],
  [-10,   0,   5,  10,  10,   5,   0, -10],
  [-10,   0,   5,  10,  10,   5,   0, -10],
  [-10,   0,   5,   5,   5,   5,   0, -10],
  [-10,   0,   0,   0,   0,   0,   0, -10],
  [-10, -30, -10, -10, -10, -10, -30, -10]]

pieceSquareBishop :: [[Int]]
pieceSquareBishop = [ -- symmetric, no need to reverse
  [-10, -10, -10, -10, -10, -10, -10, -10],
  [-10,  10,   0,   0,   0,   0,  10, -10],
  [-10,   0,   5,   5,   5,   5,   0, -10],
  [-10,   0,   5,  10,  10,   5,   0, -10],
  [-10,   0,   5,  10,  10,   5,   0, -10],
  [-10,   0,   5,   5,   5,   5,   0, -10],
  [-10,  10,   0,   0,   0,   0,  10, -10],
  [-10, -30, -10, -10, -10, -10, -30, -10]]

pieceSquareRook :: [[Int]]
pieceSquareRook = reverse [ -- favor 7th rank rooks
  [0,   0,   0,   0,   0,   0,   0,   0],
  [20, 20,  20,  20,  20,  20,  20,  20],
  [0,   0,   0,   0,   0,   0,   0,   0],
  [0,   0,   0,   0,   0,   0,   0,   0],
  [0,   0,   0,   0,   0,   0,   0,   0],
  [0,   0,   0,   0,   0,   0,   0,   0],
  [0,   0,   0,   0,   0,   0,   0,   0],
  [0,   0,   0,   0,   0,   0,   0,   0]]
