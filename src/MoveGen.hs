{-# LANGUAGE LambdaCase #-}
module MoveGen where
import Pieces
import Board
import Game
import Data.List as List
import qualified Data.Map as Map
import Data.Maybe ( isNothing )

data PossibleRay = Move [Ray] | Capture [Ray] | MoveCapture [Ray]

cardinals :: [Direction]
cardinals = [up, down, left, right]

diagonals :: [Direction]
diagonals = [upLeft, upRight, downLeft, downRight]

-- YAY FUN
getPossibleRays :: Index -> Piece -> [PossibleRay]
getPossibleRays index (Piece Pawn White) = case indexToRank index of
    Rank2 -> Move (map (\dir -> extend index dir 2) [up]) : [Capture $ map (\dir -> extend index dir 1) [upLeft, upRight]]
    _ -> Move (map (\dir -> extend index dir 1) [up]) : [Capture $ map (\dir -> extend index dir 1) [upLeft, upRight]]
getPossibleRays index (Piece Pawn Black) = case indexToRank index of
    Rank7 -> Move (map (\dir -> extend index dir 2) [down]) : [Capture $ map (\dir -> extend index dir 1) [downLeft, downRight]]
    _ -> Move (map (\dir -> extend index dir 1) [down]) : [Capture $ map (\dir -> extend index dir 1) [downLeft, downRight]]
getPossibleRays index (Piece Bishop _) = [MoveCapture $ map (\dir -> extend index dir 8) diagonals]
getPossibleRays index (Piece Knight _) = [MoveCapture $ map (\dir -> extend index dir 1) [ (-1,  2), 
                                                                                           (1,   2),
                                                                                           (2,   1),
                                                                                           (2,  -1),
                                                                                           (1,  -2),
                                                                                           (-1, -2),
                                                                                           (-2, -1),
                                                                                           (-2,  1) ]]
getPossibleRays index (Piece Rook _) = [MoveCapture $ map (\dir -> extend index dir 8) cardinals]
getPossibleRays index (Piece Queen _) = [MoveCapture $ map (\dir -> extend index dir 8) (cardinals ++ diagonals)]
getPossibleRays index (Piece King _) = [MoveCapture $ map (\dir -> extend index dir 1) (cardinals ++ diagonals)]

-- captures are valid if piece on target is opposite color
rayValidCaptures :: GameState -> Color -> Ray -> Ray
rayValidCaptures game color = filter (attacked game color)
    where
        attacked game color index = case Map.lookup index $ board game of
            Just (Piece _ other) -> other /= color
            Nothing -> False

-- ray continues as long as squares are not occupied
rayValidMoves :: GameState -> Ray -> Ray
rayValidMoves game = takeWhile (isNothing . flip Map.lookup (board game))

-- combination of above -- basically for anything that isnt a pawn lmao
rayValidCaptureMoves :: GameState -> Color -> Ray -> Ray
rayValidCaptureMoves _ _ [] = []
rayValidCaptureMoves game color (index:rest) = case Map.lookup index $ board game of
    Just (Piece _ other) -> [index | other /= color]
    Nothing              -> index : rayValidCaptureMoves game color rest

genValidRays :: GameState -> Color -> [PossibleRay] -> Ray
genValidRays game color = concatMap (\case
    Move rays -> concatMap (rayValidMoves game) rays
    Capture rays -> concatMap (rayValidCaptures game color) rays
    MoveCapture rays -> concatMap (rayValidCaptureMoves game color) rays)