module Main where

import UCIParser
import Text.Megaparsec
import System.Exit
import Control.Monad.State
import Game
import Search

main :: IO ()
main = runUCI startState
  
runUCI :: GameState -> IO ()
runUCI state = do
  line <- getLine
  case parse parseUCICmd "" line of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right cmd -> case cmd of
      UCI -> do
        putStrLn "id name Sicefish"
        putStrLn "id author Elvin Liu & Claude Zou"
        putStrLn "uciok"
      UCINewGame -> putStr "" -- do nothing?
      IsReady -> putStrLn "readyok" -- apparently this can be sent while calculating and we need to respond immediately (???)
      Position new_state -> runUCI new_state
      Go t -> putStrLn (show (searchPosition state)) -- todo: do search
      Quit -> exitSuccess
  runUCI state