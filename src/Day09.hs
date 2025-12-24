module Day09 (run) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sortBy, sort, tails, foldl')
import Data.Function ((&))

run :: IO ()
run = do
  input <- readFile "inputs/day09.txt"
  answer1 <- part1 input
  answer2 <- part2 input
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2

data Coord = Coord {
  x :: Int,
  y :: Int
} deriving (Show, Eq, Ord)

makeCoord :: String -> Coord
makeCoord s =
  Coord { x = read h, y = read $ head t }
  where
    (h:t) = splitOn "," s

getArea :: Coord -> Coord -> Int
getArea (Coord x1 y1) (Coord x2 y2) =
  (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

part1 :: String -> IO Int
part1 s = do
  let input = lines s
      coords = map makeCoord input
      areas = [ getArea c1 c2
              | (i, c1) <- zip [0 .. ] coords
              , (j, c2) <- zip [0 .. ] coords
              , i < j
              ]
  return $ maximum areas

-- tl tr br bl
data Rect = Rect Coord Coord Coord Coord
  deriving (Show)

createRect :: Coord -> Coord -> Rect
createRect (Coord x1 y1) (Coord x2 y2) =
  Rect
  (Coord (min x1 x2) (min y1 y2))
  (Coord (max x1 x2) (min y1 y2))
  (Coord (max x1 x2) (max y1 y2))
  (Coord (min x1 x2) (max y1 y2))

getRectPoints :: Rect -> [Coord]
getRectPoints (Rect c1 c2 c3 c4) = [c1,c2,c3,c4]

getRectEdges :: Rect -> [(Coord, Coord)]
getRectEdges r =
  let points = getRectPoints r
  in zip points (tail points ++ [head points])

isOnLine (Coord cx cy) (src, dst)
  | isVertical src dst = x src == cx &&
                            min (y src) (y dst) <= cy &&
                            max (y src) (y dst) >= cy
  | isHorizontal src dst = y src == cy &&
                              min (x src) (x dst) <= cx &&
                              max (x src) (x dst) >= cx
  | otherwise = error "NOT POSSIBLE"

isCoordInPolygon :: [(Coord, Coord)] -> Coord -> Bool
isCoordInPolygon vs p = odd (length $ filter (intersectsVertical p) vs) || any (isOnLine p) vs
  where
    intersectsVertical (Coord cx cy) (src, dst)
      | y dst > y src = 
          x src == x dst &&
          cx < x src &&
          cy < y dst &&
          cy >= y src
      | otherwise =
          x src == x dst &&
          cx < x src &&
          cy < y src &&
          cy >= y dst

isVertical :: Coord -> Coord -> Bool
isVertical p q = (x p) == (x q)

isHorizontal :: Coord -> Coord -> Bool
isHorizontal p q = (y p) == (y q)

cross :: Coord -> Coord -> Coord -> Coord -> Bool
cross (Coord vx1 vy1) (Coord _ vy2) (Coord hx1 hy1) (Coord hx2 _) =
  vx1 > min hx1 hx2
  && vx1 < max hx1 hx2
  && hy1 > min vy1 vy2
  && hy1 < max vy1 vy2

intersect :: (Coord, Coord) -> (Coord, Coord) -> Bool
intersect (a1, a2) (b1, b2)
  | isVertical a1 a2 && isHorizontal b1 b2 = cross a1 a2 b1 b2
  | isHorizontal a1 a2 && isVertical b1 b2 = cross b1 b2 a1 a2
  | otherwise = False


noIntersect :: [(Coord, Coord)] -> (Coord, Coord) -> Bool
noIntersect vs e = not (any (intersect e) vs)

isRectInPolygon :: Rect -> [(Coord, Coord)] -> Bool
isRectInPolygon rect polyEdges =
  let rectEdges = getRectEdges rect
      rectPoints = getRectPoints rect
  in all (isCoordInPolygon polyEdges) rectPoints &&
     all (noIntersect polyEdges) rectEdges

getRectArea :: Rect -> Int
getRectArea (Rect c1 c2 c3 c4) =
  let xs = map x [c1,c2,c3,c4]
      ys = map y [c1,c2,c3,c4]
      width  = maximum xs - minimum xs + 1
      height = maximum ys - minimum ys + 1
  in width * height

part2 :: String -> IO Int
part2 s = do
  let coords = map makeCoord $ lines s
      edges = zip coords (tail coords ++ [head coords])
      possibleRects = concatMap (\c -> map (createRect c) coords) coords
      -- testRect = createRect (Coord 9 5) (Coord 2 3)
      filteredRects = filter (`isRectInPolygon` edges) possibleRects
  -- print $ isRectInPolygon testRect edges
  -- print $ all (isCoordInPolygon edges) (getRectPoints testRect)
  -- print $ all (noIntersect edges) (getRectEdges testRect)
      ans = maximum $ map getRectArea filteredRects
  return ans

