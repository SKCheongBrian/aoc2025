module Day12 (run) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Q
import Data.Sequence (Seq (..))
import Data.List.Split (splitOn)
import Data.List (subsequences)
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict

run :: IO ()
run = do
  input <- readFile "inputs/day12.txt"
  answer1 <- part1 input 
  putStrLn $ "Part 1: " ++ show answer1

type ShapeArea = M.Map Int Int

solve :: [String] -> Int
solve ss = go ss M.empty 0 0
  where
    go [] _ _ ans = ans
    go (string:rest) mapper shapeIdx ans
      | length string > 0 && head (reverse string) == ':' = go rest mapper (read $ [head string]) ans
      | all (`elem` "#.") string =
          let
            processLine :: M.Map Int Int -> String -> Int -> M.Map Int Int
            processLine m [] _ = m
            processLine m (c:r) idx
             | c == '#' = processLine (M.insertWith (+) idx 1 m) r idx
             | otherwise = processLine m r idx
            mapper' = M.insertWith (+) shapeIdx (length (filter (== '#') string)) mapper
          in go rest mapper' shapeIdx ans
      | 'x' `elem` string =
          let
            processQuery :: String -> (Int, Int, [Int])
            processQuery strQry =
              let
                splitted = splitOn ":" strQry
                left = splitted !! 0
                width = read (splitOn "x" left !! 0) :: Int
                height = read (splitOn "x" left !! 1) :: Int
                second = drop 1 (splitted !! 1)
                nums = map read (words second) :: [Int]
              in (width,height,nums)
            query = processQuery string
            isPossible :: (Int, Int, [Int]) -> M.Map Int Int -> Bool
            isPossible (w, h, ns) m =
              let
                presentArea = sum [c * (m M.! i) | (i,c) <- zip [0..] ns]
              in presentArea <= w * h
          in if isPossible query mapper
             then go rest mapper shapeIdx (ans + 1)
             else go rest mapper shapeIdx ans
      | otherwise = go rest mapper shapeIdx ans
          

part1 :: String -> IO Int
part1 s = do
  print $ solve $ lines s
  return 0


