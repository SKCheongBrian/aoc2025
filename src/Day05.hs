module Day05 (run) where

import qualified Data.Set as Set
import Data.Function ((&))
import Data.List.Split (splitOn)

run :: IO ()
run = do
  input <- readFile "inputs/day05.txt"
  answer1 <- part1 input
  answer2 <- part2 input
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2

parseRange :: String -> (Int, Int)
parseRange s = let [start, end] = map read (splitOn "-" s) :: [Int]
               in (start, end)


-- parse :: String -> ([(Int, Int)], [Int])
-- parse s =
--   let
--     rangeAndIngre = splitOn "\n\n" s


part1 :: String -> IO Int
part1 s = do
  let [rangeString, ingString] = splitOn "\n\n" s
  let ranges = map parseRange $ lines rangeString
  let ingredients = map read $ lines ingString :: [Int]
  let freshIngredients = filter (\i -> any (\(start, end) -> i <= end && i >= start) ranges) ingredients
  return $ length freshIngredients

addTo :: Range -> Set.Set Range -> Set.Set Range
addTo rng = Set.foldr mergeRange (Set.singleton rng)
  where
    mergeRange r acc
      | overlap r rng = Set.insert (merge r rng) (Set.delete rng acc) 
      | otherwise = Set.insert r acc

type Range = (Int, Int)

overlap :: Range -> Range -> Bool
overlap (s1,e1) (s2,e2) = s1 <= e2 && s2 <= e1

merge :: Range -> Range -> Range
merge (s1, e1) (s2, e2) = (min s1 s2, max e1 e2)

step :: Set.Set Range -> Set.Set Range
step = foldr addTo Set.empty 

fix :: Eq t => (t -> t) -> t -> t
fix f x = let x' = f x in if x == x' then x else fix f x'

-- part2 :: String -> IO Int
part2 s = do
  let [rangeString, _] = splitOn "\n\n" s
  let ranges = Set.fromList (map parseRange $ lines rangeString)
  let mergedRanges = fix step ranges
  print mergedRanges
  let ingredients = map (\(start,end) -> end - start + 1) $ Set.toList mergedRanges
  return $ sum ingredients






