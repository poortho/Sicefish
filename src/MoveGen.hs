{-# LANGUAGE LambdaCase #-}
module MoveGen where
import Pieces
import Board
import Game
import Data.List as List
import Data.Maybe as Maybe
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import Data.Maybe ( isNothing )
import Move
import Special
-- for debugging purposes
import Control.Monad (mapM_)

data PossibleRay = OnlyMove [Ray] | OnlyCapture [Ray] | MoveCapture [Ray]


cardinals :: [Direction]
cardinals = [up, down, left, right]

diagonals :: [Direction]
diagonals = [upLeft, upRight, downLeft, downRight]

getRays :: PossibleRay -> [Ray]
getRays (OnlyMove rays) = rays
getRays (OnlyCapture rays) = rays
getRays (MoveCapture rays) = rays

-- YAY FUN
getPossibleRays :: Index -> Piece -> [PossibleRay]
getPossibleRays index (Piece Pawn White) = case indexToRank index of
    Rank2 -> OnlyMove (map (\dir -> extend index dir 2) [up]) : [OnlyCapture $ map (\dir -> extend index dir 1) [upLeft, upRight]]
    _ -> OnlyMove (map (\dir -> extend index dir 1) [up]) : [OnlyCapture $ map (\dir -> extend index dir 1) [upLeft, upRight]]
getPossibleRays index (Piece Pawn Black) = case indexToRank index of
    Rank7 -> OnlyMove (map (\dir -> extend index dir 2) [down]) : [OnlyCapture $ map (\dir -> extend index dir 1) [downLeft, downRight]]
    _ -> OnlyMove (map (\dir -> extend index dir 1) [down]) : [OnlyCapture $ map (\dir -> extend index dir 1) [downLeft, downRight]]
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
rayValidCaptures game color = filter $ \index -> isEnPassantIndex (enPassant game) color index || attacking game color index
    where
        attacking game color index = case HMap.lookup index (board game) of
            Just (Piece _ other) -> other /= color
            Nothing -> False

-- ray continues as long as squares are not occupied
rayValidMoves :: GameState -> Ray -> Ray
rayValidMoves game = takeWhile (isNothing . flip HMap.lookup (board game))

-- combination of above -- basically for anything that isnt a pawn lmao
rayValidCaptureMoves :: GameState -> Color -> Ray -> Ray
rayValidCaptureMoves _ _ [] = []
rayValidCaptureMoves game color (index:rest) = case HMap.lookup index (board game) of
    Just (Piece _ other) -> [index | other /= color]
    Nothing              -> index : rayValidCaptureMoves game color rest

genValidRays :: GameState -> Color -> [PossibleRay] -> Ray
genValidRays game color = concatMap (\case
    OnlyMove rays -> concatMap (rayValidMoves game) rays
    OnlyCapture rays -> concatMap (rayValidCaptures game color) rays
    MoveCapture rays -> concatMap (rayValidCaptureMoves game color) rays)

-- very yucky
isSquareAttacked :: Board -> Color -> Index -> Bool
isSquareAttacked brd color index = let oppPieces = map (`Piece` color) allPieces in
    any (\p -> any (rayAttacksSquare brd p) (getPossibleRays index p)) oppPieces
        where
            rayAttacksSquare :: Board -> Piece -> PossibleRay -> Bool
            rayAttacksSquare _ _ (OnlyMove _) = False
            rayAttacksSquare brd piece posRays = any (rayAttacksSquare' brd piece) $ getRays posRays
                where
                    rayAttacksSquare' :: Board -> Piece -> Ray -> Bool
                    rayAttacksSquare' _ _ [] = False
                    rayAttacksSquare' brd piece@(Piece pType color) (index : ray) = case HMap.lookup index brd of
                        Just (Piece otherP otherC) -> otherC /= color && otherP == pType
                        Nothing -> rayAttacksSquare' brd piece ray

isPlayerInCheck :: GameState -> Color -> Bool
isPlayerInCheck game color = isSquareAttacked (board game) color (if color == White then whiteKing game else blackKing game)

generateMoves :: GameState -> [GameState]
generateMoves game = sortOn (moveType . head . moves) $ filter (not . flip isPlayerInCheck (player game)) (concatMap (genPseudoLegal game) [(r, f) | r <- [0..7], f <- [0..7]])
    where
        genPseudoLegal :: GameState -> Index -> [GameState]
        genPseudoLegal game index = case HMap.lookup index (board game) of
            Nothing -> []
            Just piece@(Piece pType color) ->
                if color /= player game
                    then []
                    else case pType of
                        King -> processRays game index $ filter (not . isSquareAttacked (board game) (player game))
                                              (genValidRays game (player game) (getPossibleRays index piece) ++ genCastling game)
                        _    -> processRays game index $ genValidRays game (player game) (getPossibleRays index piece)

processRays :: GameState -> Index -> Ray -> [GameState]
processRays game src dests = Maybe.mapMaybe (playMove game) $ concatMap (movesFromIndexes (board game) src) dests

movesFromIndexes :: Board -> Index -> Index -> [Move]
movesFromIndexes brd src dest = case (HMap.lookup src brd, indexToRank dest) of
    (Just (Piece Pawn White), Rank8) -> map (Move src dest Promotion . Just) promotions
    (Just (Piece Pawn Black), Rank1) -> map (Move src dest Promotion . Just) promotions
    (_, _) -> case HMap.lookup dest brd of
        Nothing -> [Move src dest Quiet Nothing]
        (Just _) -> [Move src dest Capture Nothing]

genCastling :: GameState -> [Index]
genCastling game
    | isPlayerInCheck game (player game) = []
    | otherwise = concatMap (\castle -> case Map.lookup castle (canCastle game) of
                                            Just True -> genCastling' game castle
                                            _         -> []) [Castle (player game) side | side <- [Short, Long]]
        where
            genCastling' :: GameState -> Castle -> [Index]
            genCastling' game (Castle color side) = let dir = if side == Short then right else left
                                                        len = if side == Short then 2 else 3
                                                        src = if color == White then whiteKing game else blackKing game
                                                        squaresToCheckOccupied = extend src dir len
                                                        squaresToCheckCheck = extend src dir 2 in
                [last squaresToCheckCheck | not (any (isOccupied (board game)) squaresToCheckOccupied || any (isSquareAttacked (board game) (player game)) squaresToCheckCheck)]
                    where
                        isOccupied brd index = case HMap.lookup index brd of
                            Nothing -> False
                            Just _ -> True

printMoves :: [GameState] -> IO ()
printMoves g = mapM_ putStrLn $ getMoves g

getMoves :: [GameState] -> [String]
getMoves [] = []
getMoves ((GameState brd _ _ _ _ _ _ _ moves) : rest) = parseMove (head moves) brd : getMoves rest
    where
        parseMove (Move from to _ Nothing) brd = getPieceType brd to ++ " " ++ show (indexToFr from) ++ " " ++ show (indexToFr to)
        parseMove (Move from to _ (Just p)) brd = getPieceType brd to ++ " " ++ show (indexToFr from) ++ " " ++ show (indexToFr to) ++ "=" ++ show p
        getPieceType brd index = case HMap.lookup index brd of
            Nothing -> "LOLWRONG"
            Just (Piece ptype _) -> show ptype

getLastMove :: GameState -> Move
getLastMove (GameState _ _ _ _ _ _ _ _ []) = undefined -- no move L
getLastMove (GameState _ _ _ _ _ _ _ _ l) = head l

