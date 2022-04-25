module Board where

import Pieces
import Data.Vector as Vector hiding (catMaybes)
import Data.Word
import Data.Int
import Data.Bits ( Bits((.&.), shiftL, shiftR, (.|.)) )
import Data.Maybe as Maybe (catMaybes)

type Square = Maybe Piece

type Board = Vector.Vector Square

-- bit representation of coordinate, as in 0x88
type Index = Int

type Ray = [Index]

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8
    deriving (Eq, Show, Bounded, Enum)

data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH
    deriving (Eq, Show, Bounded, Enum)

type Direction = Int

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
emptyBoard = Vector.replicate 0x80 Nothing

-- as in 0x88
indexToFile :: Index -> File
indexToFile = toEnum . fromIntegral . (.&. 7)

indexToRank :: Index -> Rank
indexToRank = toEnum . fromIntegral . flip shiftR 4

indexToFr :: Index -> (File, Rank)
indexToFr bits = (indexToFile bits, indexToRank bits)

frToIndex :: File -> Rank -> Index
frToIndex file rank = fromIntegral $ (fromEnum rank `shiftL` 4) + fromEnum file

-- uses 0x88 to check if a move is out of bounds
move :: Index -> Direction -> Maybe Index
move index dir = let dest = index + fromIntegral dir in
    if (dest .&. 0x88) == 0 then Just dest else Nothing

-- generate a ray of some length in a given direction
extend :: Index -> Direction -> Int -> Ray
extend index dir len = catMaybes $ extend' (Just index) dir len
    where
        extend' :: Maybe Index -> Direction -> Int -> [Maybe Index]
        extend' _ _ 0 = []
        extend' Nothing _ _ = []
        extend' (Just index) dir len = let next = move index dir in
            next : extend' next dir (len-1)

