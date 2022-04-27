module Game where

import Pieces
import Board
import Move
import Special
import qualified Data.Map as Map

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

playMoves :: Maybe GameState -> [Move] -> Maybe GameState
playMoves Nothing _ = Nothing
playMoves state [] = state
playMoves (Just state) (x:xs) = playMoves (playMove state x) xs

startState :: GameState
startState = GameState startBoard White (0, 4) (7, 4) defaultCastling (EnPassant Nothing) 0 1