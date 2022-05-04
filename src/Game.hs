module Game where

import Pieces
import Board
import Move
import Special
import qualified Data.HashMap.Strict as Map

data GameState = GameState {
    board :: Board,
    player :: Color,
    whiteKing :: Index,
    blackKing :: Index,
    canCastle :: CanCastle,
    enPassant :: EnPassant,
    moveClock :: Int, -- for 50 move rule
    moveCounter :: Int, -- not sure if necessary, but it's in FEN
    moves :: [Move]
} deriving (Eq, Show)

data TimeControl = TimeControl {
    wtime :: Int,
    btime :: Int,
    winc :: Int,
    binc :: Int
} deriving (Eq, Show)

-- TODO :smile:
playMove :: GameState -> Move -> Maybe GameState
playMove game@(GameState brd toMove wk bk rights ep clock counter moveList) move@(Move from to _ _) = case Map.lookup from brd of
    Nothing -> Nothing -- shouldn't happen ever
    Just piece@(Piece pType pColor) -> Just $ GameState (updateBoard brd piece move ep)
                                                        (swapColor toMove)
                                                        (if from == wk then to else wk)
                                                        (if from == bk then to else bk)
                                                        (updateCastling rights brd move)
                                                        (updateEnPassant brd move)
                                                        (updateMoveClock clock brd pType to)
                                                        (counter + 1)
                                                        (moveList ++ [move])

-- insanely ugly LOL
updateBoard :: Board -> Piece -> Move -> EnPassant -> Board
updateBoard brd king@(Piece King color) (Move from to _ _) _ = case (indexToFile from, indexToFile to) of
    (FileE, FileG) -> let brd' = Map.insert to king brd
                          brd'' = Map.delete from brd'
                          rookIndex = frToIndex FileH (indexToRank from)
                          rookToIndex = frToIndex FileF (indexToRank from)
                          brd''' = Map.insert rookToIndex (Piece Rook color) brd'' in
                        Map.delete rookIndex brd'''
    (FileE, FileC) -> let brd' = Map.insert to king brd
                          brd'' = Map.delete from brd'
                          rookIndex = frToIndex FileA (indexToRank from)
                          rookToIndex = frToIndex FileD (indexToRank from)
                          brd''' = Map.insert rookToIndex (Piece Rook color) brd'' in
                        Map.delete rookIndex brd'''
    _ -> let brd' = Map.insert to king brd in
                Map.delete from brd'
updateBoard brd pawn@(Piece Pawn color) (Move from to _ (Just piece)) _ = let brd' = Map.delete from brd in
                                                        Map.insert to (Piece piece color) brd'
updateBoard brd pawn@(Piece Pawn color) (Move from to _ _) ep = case getPawnIndexEP ep color to of
    Nothing -> let brd' = Map.insert to pawn brd in
                Map.delete from brd'
    Just captured -> let brd' = Map.insert to pawn brd in
                      foldr Map.delete brd' [captured, from]
updateBoard brd piece (Move from to _ _) _ = let brd' = Map.insert to piece brd in
                                                Map.delete from brd'

updateMoveClock :: Int -> Board -> PieceType -> Index -> Int
updateMoveClock clock brd pType to
    | pType == Pawn = 0
    | otherwise = case Map.lookup to brd of
        Nothing -> clock + 1
        Just _ -> 0

playMoves :: Maybe GameState -> [Move] -> Maybe GameState
playMoves Nothing _ = Nothing
playMoves state [] = state
playMoves (Just state) (x:xs) = playMoves (playMove state x) xs

startState :: GameState
startState = GameState startBoard White (frToIndex FileE Rank1) (frToIndex FileE Rank8) defaultCastling defaultEnPassant 0 1 []