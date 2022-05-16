module Search where

import Eval
import Pieces
import Game
import Move
import MoveGen
import Data.List

searchPosition :: GameState -> (Int, Move)
searchPosition state@(GameState _ col _ _ _ _ _ _ _) = case generateMoves state of
  [] -> undefined -- position is in checkmate/stalemate
  l -> let bestMove = (if col == White then maximumBy else minimumBy) (\x y -> compare (fst x) (fst y)) (map (\x -> (alphaBeta x (-100000000) 100000000 3, x)) l) in
          (fst bestMove, getLastMove . snd $ bestMove)

alphaBeta :: GameState -> Int -> Int -> Int -> Int
alphaBeta state _ _ (-1) = evalPosition state
alphaBeta state _ _ 0 = evalPosition state
alphaBeta state@(GameState _ White _ _ _ _ _ _ _) a b depth = helperF (f $ generateMoves state) a b depth (-10000000)
  where
        helperF [] _ _ _ _ = if isPlayerInCheck state White then -9999999-depth else 0 -- never called recursively, only when states is empty
        helperF [x] a b depth val = max val (alphaBeta x a b (depth-1))
        helperF (x:xs) a b depth val = if newval >= b then newval else helperF xs (max a newval) b depth newval
          where newval = max val (alphaBeta x a b (depth-1))
        f l = map (\x -> snd x) $ sortBy (\x y -> compare (fst y) (fst x)) (map (\x -> (evalPosition x, x)) l)
alphaBeta state@(GameState _ Black _ _ _ _ _ _ _) a b depth = helperF (f $ generateMoves state) a b depth 10000000
  where
        helperF [] _ _ _ _ = if isPlayerInCheck state Black then 9999999+depth else 0
        helperF [x] a b depth val = min val (alphaBeta x a b (depth-1))
        helperF (x:xs) a b depth val = if newval <= a then newval else helperF xs a (min b newval) depth newval
          where newval = min val (alphaBeta x a b (depth-1))
        f l = map (\x -> snd x) $ sortBy (\x y -> compare (fst x) (fst y)) (map (\x -> (evalPosition x, x)) l)

minimax :: GameState -> Int -> Int
minimax state 0 = evalPosition state
minimax state@(GameState _ White _ _ _ _ _ _ _) depth = case generateMoves state of
  [] -> if isPlayerInCheck state White then -9999999 else 0
  states -> maximum (map (\x -> minimax x (depth - 1)) states)

minimax state@(GameState _ Black _ _ _ _ _ _ _) depth = case generateMoves state of
  [] -> if isPlayerInCheck state Black then 9999999 else 0
  states -> minimum (map (\x -> minimax x (depth - 1)) states)