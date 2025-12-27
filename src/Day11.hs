module Day11 (run) where

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
  input <- readFile "inputs/day11.txt"
  answer1 <- part1 input 
  answer2 <- part2 input
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2

addEdges :: M.Map String [String] -> String -> M.Map String [String]
addEdges map line =
  let (src:targetsStrings) = splitOn ":" line
      dsts = splitOn " " (tail $ head targetsStrings)
  in foldl (\acc dst -> M.insertWith (++) src [dst] acc)  map dsts

bfs :: Ord a => a -> (a -> Bool) -> (a -> [a]) -> Int
bfs start isTarget nextStates = go (Q.singleton start) (M.singleton start 1)
  where
    go Q.Empty _ = 0
    go (curr :<| q) ways
      | isTarget curr = ways M.! curr
      | otherwise =
          let currWays = ways M.! curr
              neighbors = nextStates curr

              (ways', q') = foldr update (ways, q) neighbors
              update n (w, qu) =
                case M.lookup n w of
                  Nothing ->
                    (M.insert n currWays w, qu Q.|> n)
                  Just existing ->
                    (M.insert n (existing + currWays) w, qu)
          in go q' ways'

part1 :: String -> IO Int
part1 s = do
  let adjList = foldl addEdges M.empty (lines s)
      ans = bfs "you" (== "out") (adjList M.!)
  return ans

type Key = (String, Bool, Bool)
type Cache = M.Map Key Int

dfsMemo
  :: M.Map String [String]
  -> String
  -> Bool
  -> Bool
  -> State Cache Int
dfsMemo adj curr seenDac seenFft = do
  let seenDac' = seenDac || curr == "dac"
      seenFft' = seenFft || curr == "fft"
      key = (curr, seenDac', seenFft')

  cache <- get
  case M.lookup key cache of
    Just v -> return v
    Nothing -> do
      result <-
        if curr == "out"
          then return (fromEnum (seenDac' && seenFft'))
          else do
            let neighbors = M.findWithDefault [] curr adj
            vals <- mapM (\n -> dfsMemo adj n seenDac' seenFft') neighbors
            return (sum vals)

      modify' (M.insert key result)
      return result

part2 :: String -> IO Int
part2 s = do
  let adjList = foldl addEdges M.empty (lines s)
      ans = evalState (dfsMemo adjList "svr" False False) M.empty
  return ans

