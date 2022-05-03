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
} deriving Eq

instance Show Move where
    show (Move src dst _ Nothing) = show (fst (indexToFr src)) ++ show (snd (indexToFr src)) ++ show (fst (indexToFr dst)) ++ show (snd (indexToFr dst))
    show (Move src dst mt (Just promo)) = show (Move src dst mt Nothing) ++ [pieceToChar promo]