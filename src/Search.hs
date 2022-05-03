module Search where

import Eval
import Pieces
import Game
import Move
import MoveGen
import Data.List

searchPosition :: GameState -> Move
searchPosition state = case generateMoves state of
  [] -> undefined -- why are we searching a position in check/stalemate? lmao
  l -> getLastMove (maximumBy (\x y -> compare (alphaBeta x (-10000000) 10000000 3) (alphaBeta y (-10000000) 10000000 3)) l)
    -- should refactor because it calls alphabeta multiple times on each element...

alphaBeta :: GameState -> Int -> Int -> Int -> Int
alphaBeta state _ _ 0 = evalPosition state
alphaBeta state@(GameState _ White _ _ _ _ _ _ _) a b depth = helperF (generateMoves state) a b depth (-10000000)
  where
        helperF [] _ _ _ _ = if isPlayerInCheck state White then -9999999 else 0 -- never called recursively, only when states is empty
        helperF [x] a b depth val = max val (alphaBeta x a b (depth-1))
        helperF (x:xs) a b depth val = if newval >= b then newval else helperF xs (max a newval) b depth newval
          where newval = max val (alphaBeta x a b (depth-1))
alphaBeta state@(GameState _ Black _ _ _ _ _ _ _) a b depth = helperF (generateMoves state) a b depth 10000000
  where
        helperF [] _ _ _ _ = if isPlayerInCheck state Black then 9999999 else 0
        helperF [x] a b depth val = min val (alphaBeta x a b (depth-1))
        helperF (x:xs) a b depth val = if newval <= a then newval else helperF xs a (min b newval) depth newval
          where newval = min val (alphaBeta x a b (depth-1))

minimax :: GameState -> Int -> Int
minimax state 0 = evalPosition state
minimax state@(GameState _ White _ _ _ _ _ _ _) depth = case generateMoves state of
  [] -> if isPlayerInCheck state White then -9999999 else 0
  states -> maximum (map (\x -> minimax x (depth - 1)) states)

minimax state@(GameState _ Black _ _ _ _ _ _ _) depth = case generateMoves state of
  [] -> if isPlayerInCheck state Black then 9999999 else 0
  states -> minimum (map (\x -> minimax x (depth - 1)) states)