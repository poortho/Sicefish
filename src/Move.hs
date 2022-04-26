module Move where

import Pieces
import Board

data MoveType = Capture | Promotion | Quiet
    deriving (Eq, Show)

data Move = Move {
    src :: Index,
    dest :: Index,
    moveType :: MoveType,
    promoteTo :: Maybe PieceType
} deriving (Eq, Show)