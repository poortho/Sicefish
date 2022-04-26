module Pieces where

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Ord, Eq, Show)

data Color = White | Black
    deriving (Eq, Show, Enum, Ord)

data Piece = Piece PieceType Color
    deriving (Eq, Show)

promotions :: [PieceType]
promotions = [Bishop, Knight, Rook, Queen]

allPieces :: [PieceType]
allPieces = [Pawn, Bishop, Knight, Rook, Queen, King]

swapColor :: Color -> Color
swapColor White = Black
swapColor Black = White

