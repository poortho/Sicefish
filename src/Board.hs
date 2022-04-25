import Pieces
import Data.Vector as Vector

newtype Square = Square (Maybe Piece)
    deriving (Eq, Show)



newtype Board = Board (Vector.Vector Square)

-- bit representation of coordinate, as in 0x88
newtype Index = Word8

type Ray = [Index]

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8

data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH

newtype Direction = Int8

up :: Direction
up = 0x10

down :: Direction
down = -0x10

right :: Direction
right = 0x1

left :: Direction
left = -0x1

-- 0x88 is length 128 vector
emptyBoard :: Board
emptyBoard = Board $ Vector.replicate 0x80 Nothing

-- as in 0x88
indexToFile :: Index -> File
indexToFile = toEnum . fromIntegral . (.&. 7)

indexToRank :: Index -> Rank
indexToRank = toEnum . fromIntegral . shiftR 4

indexToFr :: Index -> (File, Rank)
indexToFr bits = (indexToFile bits, indexToRank bits)

frToIndex :: File -> Rank -> Index
frToIndex file rank = fromIntegral $ (fromEnum rank `shiftL` 4) + fromEnum file

-- 8x8 coordinate
indexToCoord :: Index -> Word8
indexToCoord index = (index .&. 7) .|. ((index .&. 0x70) `shiftR` 1)

-- uses 0x88 to check if a move is out of bounds
move :: Index -> Direction -> Maybe Index
move index dir = let dest = index + fromIntegral dir in
    if (dest .&. 0x88) == 0 then Just dest else Nothing

-- generate a ray of some length in a given direction
extend :: Index -> Direction -> Int -> Ray
extend index dir len = catMaybe $ extend' index dir len
    where
        extend :: Maybe Index -> Direction -> Int -> Ray
        extend' _ _ 0 = []
        extend' Nothing _ _ = []
        extend' index dir len = move index dir : extend' index dir len

