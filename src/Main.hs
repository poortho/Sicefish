module Main where

import UCIParser
import Text.Megaparsec
import System.Exit
import Control.Monad.State
import Game
import Search
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runUCI startState

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
      Go t -> let (score, bestMove) = searchPosition state in
                putStrLn ("info depth 5 score cp " ++ show score) >> putStrLn ("bestmove " ++ show bestMove) -- todo: do search
      Quit -> exitSuccess
      None -> putStr ""
  runUCI state