module Special where

import Pieces
import Board
import Move
import qualified Data.Map as Map

data CastleType = Short | Long
    deriving (Show, Eq, Ord)

data Castle = Castle Color CastleType
    deriving (Show, Eq, Ord)

type CanCastle = Map.Map Castle Bool

newtype EnPassant = EnPassant (Maybe Index)
    deriving (Show, Eq)

updateCastleFromCapture :: CanCastle -> Board -> Move -> CanCastle
updateCastleFromCapture rights brd move = case Map.lookup (dest move) brd of
    Just (Piece Rook color) -> case (indexToFr (dest move), color) of
                                ((FileA, Rank1), White) -> Map.insert (Castle White Long) False rights
                                ((FileH, Rank1), White) -> Map.insert (Castle White Short) False rights
                                ((FileA, Rank8), Black) -> Map.insert (Castle Black Long) False rights
                                ((FileH, Rank8), Black) -> Map.insert (Castle Black Short) False rights
                                _ -> rights
    _ -> rights

updateCastleFromMove :: CanCastle -> Board -> Move -> CanCastle
updateCastleFromMove rights brd move = case Map.lookup (src move) brd of
    Just (Piece King color) -> foldr (`Map.insert` False) rights [Castle color Long, Castle color Short]
    Just (Piece Rook color) -> case indexToFile (src move) of
        FileA -> Map.insert (Castle color Long) False rights
        FileH -> Map.insert (Castle color Short) False rights
        _ -> rights
    _ -> rights

updateCastling :: CanCastle -> Board -> Move -> CanCastle
updateCastling rights brd move = let updateCapture = updateCastleFromCapture rights brd move in
                            updateCastleFromMove updateCapture brd move

defaultCastling :: CanCastle
defaultCastling = foldr (`Map.insert` True) Map.empty [Castle color side | color <- [White, Black], side <- [Short, Long]]