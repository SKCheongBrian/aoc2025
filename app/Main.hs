module Main (main) where

import qualified Day01
import qualified Day02

main :: IO ()
main = do
  putStrLn "Which day? (1-25)"
  choice <- getLine
  case choice of
    "1" -> Day01.run
    "2" -> Day02.run
    _   -> putStrLn "Invalid day"
