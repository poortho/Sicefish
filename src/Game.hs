module Game where

import Pieces
import Board


data GameState = GameState {
    board :: Board,
    player :: Color
}