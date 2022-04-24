import Data.Bits(shiftL, shiftR, (.&.))

data PieceType = Pawn | Rook | Bishop | Knight | Rook | Queen | King
    deriving (Eq, Show, Enum)

instance Enum Piece where
    -- last bit is color, rest is piece type
    fromEnum (Piece color type) = ((fromEnum type) `shiftL` 1) + (fromEnum k)
    
    -- unwrap above
    toEnum bits = Piece (toEnum (bits .&. 1)) (toEnum (bits) `shiftR` 1)

data Color = White | Black
    deriving (Eq, Show, Enum)

data Piece = Piece PieceType Color
    deriving (Eq, Show)

promotions :: [Piece]
promotions = [Bishop, Knight, Rook, Queen]

allPieces :: [Piece]
allPieces = [Pawn, Rook, Bishop, Knight, Rook, Queen, King]

swapColor :: Color -> Color
swapColor White = Black
swapColor Black = White

