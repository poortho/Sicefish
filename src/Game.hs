module Game where

import Pieces
import Board
import Move
import Special

data GameState = GameState {
    board :: Board,
    player :: Color,
    whiteKing :: Index,
    blackKing :: Index,
    canCastle :: CanCastle,
    enPassant :: EnPassant,
    moveClock :: Int, -- for 50 move rule
    moveCounter :: Int -- not sure if necessary, but it's in FEN
} deriving (Eq, Show)

-- TODO :smile:
playMove :: GameState -> Move -> Maybe GameState
playMove (GameState brd toMove wk bk _ _ _ _) move@(Move src _ _ _) = undefined
