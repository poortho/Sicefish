module Board where

import Pieces
import qualified Data.Map as Map
import Data.Maybe as Maybe (catMaybes)
import Data.List

type Board = Map.Map (Int, Int) Piece

-- bit representation of coordinate, as in 0x88
type Index = (Int, Int)

type Ray = [Index]

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8
    deriving (Eq, Show, Bounded, Enum)

data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH
    deriving (Eq, Show, Bounded, Enum)

type Direction = (Int, Int)


up :: Direction
up = (0, 1)

down :: Direction
down = (0, -1)

right :: Direction
right = (1, 0)

left :: Direction
left = (-1, 0)

upLeft :: Direction
upLeft = (-1, 1)

upRight :: Direction
upRight = (1, 1)

downLeft :: Direction
downLeft = (-1, -1)

downRight :: Direction
downRight = (1, -1)

emptyBoard :: Board
emptyBoard = Map.empty

-- oh these are partial now. o well lmao
indexToFile :: Index -> File
indexToFile = toEnum . fst

indexToRank :: Index -> Rank
indexToRank = toEnum . snd

indexToFr :: Index -> (File, Rank)
indexToFr index = (indexToFile index, indexToRank index)

frToIndex :: File -> Rank -> Index
frToIndex file rank = (fromEnum file, fromEnum rank)

-- uses 0x88 to check if a move is out of bounds
move :: Index -> Direction -> Maybe Index
move (xInd, yInd) (xDir, yDir) = let dest = (xInd + xDir, yInd + yDir) in
    if (xInd + xDir > 7) || (xInd + xDir < 0) || (yInd + yDir > 7) || (yInd + yDir < 0) then Nothing else Just dest

-- generate a ray of some length in a given direction
extend :: Index -> Direction -> Int -> Ray
extend index dir len = catMaybes $ extend' (Just index) dir len
    where
        extend' :: Maybe Index -> Direction -> Int -> [Maybe Index]
        extend' _ _ 0 = []
        extend' Nothing _ _ = []
        extend' (Just index) dir len = let next = move index dir in
            next : extend' next dir (len-1)

startBoard :: Board
startBoard = Map.fromList $ [((0, 0), Piece Rook White), ((1, 0), Piece Knight White), ((2, 0), Piece Bishop White), ((3, 0), Piece Queen White),
                            ((4, 0), Piece King White), ((5, 0), Piece Bishop White), ((6, 0), Piece Knight White), ((7, 0), Piece Rook White)] ++
                            [((0, 7), Piece Rook Black), ((1, 7), Piece Knight Black), ((2, 7), Piece Bishop Black), ((3, 7), Piece Queen Black),
                            ((4, 7), Piece King Black), ((5, 7), Piece Bishop Black), ((6, 7), Piece Knight Black), ((7, 7), Piece Rook Black)] ++
                            [((i, 1), Piece Pawn White) | i <- [0..7]] ++ [((i, 6), Piece Pawn Black) | i <- [0..7]]