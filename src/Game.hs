module Game where

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

data GameState = GameState {
    board :: Board,
    player :: Color,
    whiteKing :: Index,
    blackKing :: Index,
    enPassant :: Maybe Index,
    moveClock :: Int, -- for 50 move rule
    moveCounter :: Int -- not sure if necessary, but it's in FEN
} deriving (Eq, Show)

-- TODO :smile:
playMove :: GameState -> Move -> Maybe GameState
playMove (GameState brd toMove wk bk) move@(Move src _ _ _) = undefined
