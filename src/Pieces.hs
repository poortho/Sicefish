module Pieces where

import Data.Function.Memoize

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Ord, Eq, Show, Enum, Bounded)

data Color = White | Black
    deriving (Eq, Show, Enum, Ord, Bounded)

data Piece = Piece PieceType Color
    deriving (Eq, Show, Bounded)

instance Memoizable Piece where
    memoize = memoizeFinite

instance Enum Piece where
    toEnum i
        | i < 6 = Piece (toEnum i) White
        | i < 12 = Piece (toEnum $ i-6) Black
        | otherwise = undefined

    fromEnum (Piece pt White) = fromEnum pt
    fromEnum (Piece pt Black) = 6 + fromEnum pt

promotions :: [PieceType]
promotions = [Bishop, Knight, Rook, Queen]

allPieces :: [PieceType]
allPieces = [Pawn, Bishop, Knight, Rook, Queen, King]

swapColor :: Color -> Color
swapColor White = Black
swapColor Black = White

pieceToChar :: PieceType -> Char
pieceToChar pt = case pt of
    Bishop -> 'b'
    Knight -> 'n'
    Rook -> 'r'
    Queen -> 'q'
    _ -> undefined -- only called for promotion