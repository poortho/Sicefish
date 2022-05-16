module Eval where

import Pieces
import Game
import Board
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

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
  Bishop -> 310
  Rook -> 500
  Queen -> 900
  King -> 0 -- only 1 king and both sides always have it

pieceSquare :: GameState -> Int
pieceSquare (GameState board _ _ _ _ _ _ _ _) = Map.foldrWithKey (\k v a -> a + pieceSquareCalc k v) 0 board

pieceSquareCalc :: Index -> Piece -> Int
pieceSquareCalc (i, j) (Piece pt col) = case pt of
  Knight -> g $ pieceSquareKnight V.! f j V.! i
  Pawn -> g $ pieceSquarePawn V.! f j V.! i
  Bishop -> g $ pieceSquareBishop V.! f j V.! i
  Rook -> g $ pieceSquareBishop V.! f j V.! i
  _ -> 0
  where f = if col == White then id else (7 -)
        g = if col == White then id else negate

pieceSquarePawn :: V.Vector (V.Vector Int)
pieceSquarePawn = V.fromList $ reverse [ -- reverse it since (0,0) is a1 in our code
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0],
  V.fromList [5,  10,  15,  20,  20,  15,  10,   5],
  V.fromList [4,   8,  12,  16,  16,  12,   8,   4],
  V.fromList [3,   6,   9,  12,  12,   9,   6,   3],
  V.fromList [2,   4,   6,   8,   8,   6,   4,   2],
  V.fromList [1,   2,   3, -10, -10,   3,   2,   1],
  V.fromList [0,   0,   0, -40, -40,   0,   0,   0],
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0]]

pieceSquareKnight :: V.Vector (V.Vector Int)
pieceSquareKnight = V.fromList [ -- symmetric, no need to reverse
  V.fromList [-10, -10, -10, -10, -10, -10, -10, -10],
  V.fromList [-10,   0,   0,   0,   0,   0,   0, -10],
  V.fromList [-10,   0,   5,   5,   5,   5,   0, -10],
  V.fromList [-10,   0,   5,  10,  10,   5,   0, -10],
  V.fromList [-10,   0,   5,  10,  10,   5,   0, -10],
  V.fromList [-10,   0,   5,   5,   5,   5,   0, -10],
  V.fromList [-10,   0,   0,   0,   0,   0,   0, -10],
  V.fromList [-10, -30, -10, -10, -10, -10, -30, -10]]

pieceSquareBishop :: V.Vector (V.Vector Int)
pieceSquareBishop = V.fromList [ -- symmetric, no need to reverse
  V.fromList [-10, -10, -10, -10, -10, -10, -10, -10],
  V.fromList [-10,  10,   0,   0,   0,   0,  10, -10],
  V.fromList [-10,   0,   5,   5,   5,   5,   0, -10],
  V.fromList [-10,   0,   5,  10,  10,   5,   0, -10],
  V.fromList [-10,   0,   5,  10,  10,   5,   0, -10],
  V.fromList [-10,   0,   5,   5,   5,   5,   0, -10],
  V.fromList [-10,  10,   0,   0,   0,   0,  10, -10],
  V.fromList [-10, -30, -10, -10, -10, -10, -30, -10]]

pieceSquareRook :: V.Vector (V.Vector Int)
pieceSquareRook = V.fromList $ reverse [ -- favor 7th rank rooks
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0],
  V.fromList [20, 20,  20,  20,  20,  20,  20,  20],
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0],
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0],
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0],
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0],
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0],
  V.fromList [0,   0,   0,   0,   0,   0,   0,   0]]
