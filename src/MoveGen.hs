{-# LANGUAGE LambdaCase #-}
module MoveGen where
import Pieces
import Board
import Game
import Data.List as List
import Data.Vector ( (!) )
import Data.Maybe ( isNothing )

data PossibleRay = Move [Ray] | Capture [Ray] | MoveCapture [Ray]

cardinals :: [Direction]
cardinals = [up, down, left, right]

diagonals :: [Direction]
diagonals = [up + left, up + right, down + left, down + right]

-- YAY FUN
getPossibleRays :: Index -> Piece -> [PossibleRay]
getPossibleRays index (Piece Pawn White) = case indexToRank index of
    Rank2 -> Move (map (\dir -> extend index dir 2) [up]) : [Capture $ map (\dir -> extend index dir 1) [up+left, up+right]]
    _ -> Move (map (\dir -> extend index dir 1) [up]) : [Capture $ map (\dir -> extend index dir 1) [up+left, up+right]]
getPossibleRays index (Piece Pawn Black) = case indexToRank index of
    Rank7 -> Move (map (\dir -> extend index dir 2) [down]) : [Capture $ map (\dir -> extend index dir 1) [down+left, down+right]]
    _ -> Move (map (\dir -> extend index dir 1) [down]) : [Capture $ map (\dir -> extend index dir 1) [down+left, down+right]]
getPossibleRays index (Piece Bishop _) = [MoveCapture $ map (\dir -> extend index dir 8) diagonals]
getPossibleRays index (Piece Knight _) = [MoveCapture $ map (\dir -> extend index dir 1) [ 2*up + left, 
                                                                                             2*up + right,
                                                                                             2*right + up,
                                                                                             2*right + down,
                                                                                             2*down + right,
                                                                                             2*down + left,
                                                                                             2*left + down,
                                                                                             2*left + up ]]
getPossibleRays index (Piece Rook _) = [MoveCapture $ map (\dir -> extend index dir 8) cardinals]
getPossibleRays index (Piece Queen _) = [MoveCapture $ map (\dir -> extend index dir 8) (cardinals ++ diagonals)]
getPossibleRays index (Piece King _) = [MoveCapture $ map (\dir -> extend index dir 1) (cardinals ++ diagonals)]

-- captures are valid if piece on target is opposite color
rayValidCaptures :: GameState -> Color -> Ray -> Ray
rayValidCaptures game color = filter (attacked game color)
    where
        attacked game color index = case board game ! index of
            Just (Piece _ other) -> other /= color
            Nothing -> False

-- ray continues as long as squares are not occupied
rayValidMoves :: GameState -> Ray -> Ray
rayValidMoves game = takeWhile (isNothing . (!) (board game))

-- combination of above -- basically for anything that isnt a pawn lmao
rayValidCaptureMoves :: GameState -> Color -> Ray -> Ray
rayValidCaptureMoves _ _ [] = []
rayValidCaptureMoves game color (index:rest) = case board game ! index of
    Just (Piece _ other) -> [index | other /= color]
    Nothing              -> index : rayValidCaptureMoves game color rest

genValidRays :: GameState -> Color -> [PossibleRay] -> Ray
genValidRays game color = concatMap (\case
    Move rays -> concatMap (rayValidMoves game) rays
    Capture rays -> concatMap (rayValidCaptures game color) rays
    MoveCapture rays -> concatMap (rayValidCaptureMoves game color) rays)