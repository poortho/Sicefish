{-# LANGUAGE LambdaCase #-}
module MoveGen where
import Pieces
import Board
import Game
import Data.List as List
import Data.Vector ( (!) )
import Data.Maybe ( isNothing )

-- rays of a given length
data LimitRay = LimitRay [Direction] Int

-- pawns capture differently than movement
data PieceMoveType = Same LimitRay | Different LimitRay LimitRay

data InboundMove = Move [Ray] | Capture [Ray] | MoveCapture [Ray]

cardinals :: [Direction]
cardinals = [up, down, left, right]

diagonals :: [Direction]
diagonals = [up + left, up + right, down + left, down + right]

getInboundMove :: Index -> Piece -> [InboundMove]
getInboundMove index piece = case getPieceMoveType index piece of
    Same (LimitRay dirs len) -> [MoveCapture $ map (\dir -> extend index dir len) dirs]
    Different (LimitRay dirsMove lenMove) 
              (LimitRay dirsAttack lenAttack) -> Move (map (\dir -> extend index dir lenMove) dirsMove)
                                                 : [Capture $ map (\dir -> extend index dir lenAttack) dirsAttack]
    where
        getPieceMoveType :: Index -> Piece -> PieceMoveType
        getPieceMoveType index (Piece Pawn color) = 
            let pawnAttack = LimitRay [up+left, up+right] 1 in
                case (indexToRank index, color) of
                    (Rank2, White) -> Different (LimitRay [up] 2) pawnAttack
                    (_, White) -> Different (LimitRay [up] 1) pawnAttack
                    (Rank7, Black) -> Different (LimitRay [down] 2) pawnAttack
                    (_, Black) -> Different (LimitRay [down] 1) pawnAttack
        getPieceMoveType _ (Piece Bishop _) = Same $ LimitRay diagonals 8
        getPieceMoveType _ (Piece Knight _) = Same $ LimitRay [ 2*up + left, 
                                                                2*up + right,
                                                                2*right + up,
                                                                2*right + down,
                                                                2*down + right,
                                                                2*down + left,
                                                                2*left + down,
                                                                2*left + up ] 1
        getPieceMoveType _ (Piece Rook _) = Same $ LimitRay cardinals 8
        getPieceMoveType _ (Piece Queen _) = Same $ LimitRay (cardinals ++ diagonals) 8
        getPieceMoveType _ (Piece King _) = Same $ LimitRay (cardinals ++ diagonals) 1

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

genValidRays :: GameState -> Color -> [InboundMove] -> Ray
genValidRays game color = concatMap (\case
    Move rays -> concatMap (rayValidMoves game) rays
    Capture rays -> concatMap (rayValidCaptures game color) rays
    MoveCapture rays -> concatMap (rayValidCaptureMoves game color) rays)