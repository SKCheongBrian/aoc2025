module Day07 (run) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function ((&))

run :: IO ()
run = do
  input <- readFile "inputs/day07.txt"
  answer1 <- part1 input
  answer2 <- part2 input
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2

getStart :: String -> Set.Set Int
getStart = getSet' 0
  where
    getSet' _ [] = Set.empty
    getSet' i ('S':_) = Set.singleton i
    getSet' i (_:rest) = getSet' (i + 1) rest

toSet :: String -> Set.Set Int
toSet = toSet' 0 Set.empty
  where
    toSet' _ set [] = set
    toSet' i set ('^':rest) = toSet' (i+1) (set `Set.union` Set.singleton i) rest
    toSet' i set (_:rest) = toSet'(i+1) set rest
    
step :: (Int, Set.Set Int) -> Set.Set Int -> (Int, Set.Set Int)
step (count, beams) splitters =
  let splitCount = Set.size $ beams `Set.intersection` splitters
      newBeams = Set.foldr step Set.empty beams
        where
          step i acc
            | i `Set.member` splitters =
                Set.insert (i - 1) (Set.insert (i + 1) acc)
            | otherwise =
                Set.insert i acc
  in (count + splitCount, newBeams)

part1 :: String -> IO Int
part1 s = do
  let (first:splittersString) = lines s
      initialBeam = getStart first
      splittersSets = map toSet splittersString
  let ans = fst $ foldl step (0, initialBeam) splittersSets
  return ans
  
getStartMap :: String -> Map.Map Int Int
getStartMap = go 0
  where
    go _ [] = Map.empty
    go i ('S':_) = Map.singleton i 1
    go i (c:rest) = go (i + 1) rest

step2 :: Map.Map Int Int -> Set.Set Int -> Map.Map Int Int
step2 beamMap splitters =
  Map.foldrWithKey go Map.empty beamMap
  where
    go key count acc
      | key `Set.member` splitters =
          let acc1 = Map.insertWith (+) (key - 1) count acc
          in  Map.insertWith (+) (key + 1) count acc1
      | otherwise =
          Map.insertWith (+) key count acc       

part2 :: String -> IO Int
part2 s = do
  let (first:splittersString) = lines s
      initialBeam = getStartMap first
      splittersSets = map toSet splittersString
        
  let finalBeamMap = foldl step2 initialBeam splittersSets
  pure (sum (Map.elems finalBeamMap))

