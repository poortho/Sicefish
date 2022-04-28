module Main where

import UCIParser
import Text.Megaparsec
import System.Exit

main :: IO ()
main = do
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
      Position state -> putStrLn "game state todo"
      Go t -> putStrLn "go todo"
      Quit -> exitSuccess
  main