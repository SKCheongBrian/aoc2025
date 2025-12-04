module Day04 (run) where

import qualified Data.Set as Set
import Data.Function ((&))

run :: IO ()
run = do
  input <- readFile "inputs/day04.txt"
  answer1 <- part1 input
  let
    answer2 = part2 input
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2

type Coord = (Int, Int)

neighbors :: Coord -> [Coord]
neighbors (x, y)  = [ (x + 1, y)
                    , (x - 1, y)
                    , (x, y - 1)
                    , (x, y + 1)
                    , (x + 1, y + 1)
                    , (x + 1, y - 1)
                    , (x - 1, y + 1)
                    , (x - 1, y - 1)
                    ]

makeSet :: [String] -> Set.Set Coord
makeSet grid = foldr processRow Set.empty (zip [0..] grid)
  where
    processRow :: (Int, [Char]) -> Set.Set Coord -> Set.Set Coord
    processRow (y, row) acc = foldr processCell acc (zip [0..] row)
      where
        processCell (x, c) acc' = if c == '@'
          then
            Set.insert (x, y) acc'
          else
            acc'

countBy :: (Coord -> Bool) -> [Coord] -> Int
countBy _ [] = 0
countBy p (n:ns) = (if p n then 1 else 0) + countBy p ns

reachable :: Set.Set Coord -> Set.Set Coord
reachable rolls = Set.filter (\x -> countBy (`Set.member` rolls) (neighbors x) < 4) rolls

solve :: Set.Set Coord -> IO Int
solve s = do
  print s
  let f = reachable s
  print f
  return $ Set.size f

-- solve2 :: Set.Set Coord -> IO Int
solve2 :: Set.Set Coord -> Int
solve2 s = if reachable s == Set.empty 
  then 0
  else
    let reachableSet = reachable s
    in Set.size reachableSet + solve2 (Set.difference s reachableSet)
  

part1 :: String -> IO Int
part1 s = do
  print s
  s & lines & makeSet & solve

-- part2 :: String -> IO Int
part2 s = do
  s & lines & makeSet & solve2
