import Data.Bits(shiftL, shiftR, (.&.))

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Eq, Show, Enum)

data Color = White | Black
    deriving (Eq, Show, Enum)

data Piece = Piece PieceType Color
    deriving (Eq, Show)

instance Enum Piece where
    -- last bit is color, rest is piece type
    fromEnum (Piece color piece) = (fromEnum piece `shiftL` 1) + fromEnum color
    
    -- unwrap above
    toEnum bits = Piece (toEnum (bits .&. 1)) (toEnum (bits `shiftR` 1))

promotions :: [PieceType]
promotions = [Bishop, Knight, Rook, Queen]

allPieces :: [PieceType]
allPieces = [Pawn, Bishop, Knight, Rook, Queen, King]

swapColor :: Color -> Color
swapColor White = Black
swapColor Black = White

