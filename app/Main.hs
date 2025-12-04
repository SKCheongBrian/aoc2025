module Main (main) where

import qualified Day01
import qualified Day02
import qualified Day04

main :: IO ()
main = do
  putStrLn "Which day? (1-25)"
  choice <- getLine
  case choice of
    "1" -> Day01.run
    "2" -> Day02.run
    "3" -> Day03.run
    "4" -> Day04.run
    _   -> putStrLn "Invalid day"
