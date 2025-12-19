module Day08 (run) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sortBy, sort)
import Data.Function ((&))

run :: IO ()
run = do
  input <- readFile "inputs/day08.txt"
  answer1 <- part1 input
  answer2 <- part2 input
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2

data Coord = Coord { x :: Int, y :: Int, z :: Int }
  deriving (Show, Eq)

instance Ord Coord where
  compare (Coord x1 y1 z1) (Coord x2 y2 z2) =
    compare (x1, y1, z1) (x2, y2, z2)

parseCoord :: String -> Coord
parseCoord s =
  let [x,y,z] = map read (splitOn "," s)
  in Coord x y z

distance :: Coord -> Coord -> Double
distance (Coord x1 y1 z1) (Coord x2 y2 z2) =
  sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

pairwiseDistances cs =
  [ (distance c1 c2, c1, c2) | (i, c1) <- zip [0..] cs
                   , (j, c2) <- zip [0..] cs
                   , i < j ]

data UF a = UF
  { parent :: M.Map a a
  , size :: M.Map a Int -- only for roots
  } deriving (Show, Eq)

initUF :: Ord a => [a] -> UF a
initUF xs = UF
  { parent = M.fromList [(x,x) | x <- xs]
  , size = M.fromList [(x,1) | x <- xs]
  }

find :: Ord a => a -> UF a -> a
find x uf = case M.lookup x (parent uf) of
  Just p |  p /= x -> find p uf
  _ -> x

union :: Ord a => a -> a -> UF a -> UF a
union x y uf
  | rx == ry = uf
  | sx < sy = attach rx ry
  | otherwise = attach ry rx
  where
    rx = find x uf
    ry = find y uf
    sx = size uf M.! rx
    sy = size uf M.! ry

    attach small big =
      uf
        { parent = M.insert small big (parent uf)
        , size = M.insert big (sx + sy) (size uf)
        }


connect :: Int -> UF Coord -> [(Double, Coord, Coord)] -> UF Coord
connect 0 uf _ = uf
connect _ uf [] = uf
connect n uf ((_, c1, c2) : rest) = connect (n - 1) (union c1 c2 uf) rest

componentSizes :: Ord a => UF a -> [Int]
componentSizes uf = 
  [ size uf M.! r
  | r <- M.keys (parent uf)
  , parent uf M.! r == r
  ]

componentSize :: Ord a => a -> UF a -> Int
componentSize a uf = size uf M.! find a uf

part1 :: String -> IO Int
part1 s = do
  let rawLines = lines s
      coords = map parseCoord rawLines
      distances = sortBy (\(d1,_,_) (d2, _, _) -> compare d1 d2) $ pairwiseDistances coords
      initialUF = initUF coords
      uf = connect 10 initialUF distances
      sizes = componentSizes uf
  print sizes
  return $ product $ take 3 $ reverse $ sort sizes

process :: UF Coord -> Int -> [(Double, Coord, Coord)] -> Int
process _ _ [] = 0 -- this should never happen
process uf n ((_, c1, c2):rest)
  | find c1 uf == find c2 uf
  = process uf n rest

  | otherwise =
      let uf' = union c1 c2 uf
      in if componentSize c1 uf' == n
            then x c1 * x c2
         else process uf' n rest
  
part2 :: String -> IO Int
part2 s = do
  let rawLines = lines s
      coords = map parseCoord rawLines
      distances = sortBy (\(d1,_,_) (d2, _, _) -> compare d1 d2) $ pairwiseDistances coords
      initialUF = initUF coords
      ans = process initialUF (length coords) distances
  return ans

