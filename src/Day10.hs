module Day10 (run) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Q
import Data.Sequence (Seq (..))
import Data.List.Split (splitOn)
import Data.List (subsequences)
import Data.Maybe (fromMaybe)

run :: IO ()
run = do
  input <- readFile "inputs/day10.txt"
  answer1 <- part1 input
  answer2 <- part2 input
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2

data Light = On | Off
  deriving (Show, Eq, Ord)

type Diagram = [Light]

type Move = [Int]

parseDiagram :: String -> ([Light], String)
parseDiagram [] = error "cannot be"
parseDiagram ('[':rest) = go rest []
  where go [] _ = error "also cannot be"
        go (']':rem) acc = (reverse acc, rem)
        go ('.':rem) acc = go rem (Off:acc)
        go ('#':rem) acc = go rem (On:acc)
        go _ _ = error "cannot be 4"
parseDiagram _ = error "cannot be 3"

parseMoves :: String -> ([Move], String)
parseMoves s = (map parseGroups groups, rest)
  where
    groups = extractGroups s
    rest = dropWhile (/= '{') s

extractGroups :: String -> [String]
extractGroups [] = []
extractGroups ('(':xs) =
  let (grp, rest) = span (/= ')') xs
  in grp : extractGroups (drop 1 rest)
extractGroups (_:xs) = extractGroups xs

parseGroups :: String -> Move
parseGroups  = map read . splitOn ","

parseTarget :: String -> Joltage
parseTarget s =
  let numStrWithCurl = span (/= '}') s
      numStr = drop 1 $ dropWhile (/= '{') (fst numStrWithCurl)
  in map read (splitOn "," numStr)

processLine :: String -> (Diagram, [Move])
processLine s =
  let (diagram, s') = parseDiagram s
      (moves, _) = parseMoves s'
  in (diagram, moves)

bfs :: Ord a => a -> (a -> Bool) -> (a -> [a]) -> Maybe Int
bfs start isTarget nextStates =
  go (Q.singleton (start, 0)) (S.singleton start)
  where
    go Q.Empty _ = Nothing
    go ((x, d) :<| q) visited
      | isTarget x = Just d
      | otherwise =
          let neighbors = filter (`S.notMember` visited) (nextStates x)
              visited' = foldr S.insert visited neighbors
              q' = q <> Q.fromList [(n, d + 1) | n <- neighbors]
          in go q' visited'

applyMove :: Diagram -> Move -> Diagram
applyMove diagram move =
  [ if i `elem` move then flipLight l else l
  | (i, l) <- zip [0..] diagram
  ]
  where
    flipLight On = Off
    flipLight Off = On

genDiagrams :: Diagram -> [Move] -> [Diagram]
genDiagrams diagram = map (applyMove diagram)

part1 :: String -> IO Int
part1 s = do
  let machines = map processLine $ lines s
      steps = map (\(diagram, moves) -> bfs [Off | _ <- diagram]
                    (== diagram)
                    (`genDiagrams` moves)) machines
      ans = foldl' (\acc m -> acc + fromMaybe 0 m) 0 steps
  return ans

type Button = [Int]
type Joltage = [Int]

processLine2 :: String -> (Joltage, [Button])
processLine2 s =
  let (_, s') = parseDiagram s
      (moves, s'') = parseMoves s'
      target = parseTarget s''
  in (target, moves)

type Parity = [Int]  -- 0 or 1

parity :: Joltage -> Parity
parity = map (`mod` 2)

xorParity :: Int -> [Button] -> Parity
xorParity n bs =
  [ sum [ if i `elem` b then 1 else 0 | b <- bs ] `mod` 2
  | i <- [0..n-1]
  ]

buildParityMap :: Int -> [Button] -> M.Map Parity [[Button]]
buildParityMap n buttons =
  M.fromListWith (++)
    [ (xorParity n bs, [bs]) | bs <- subsequences buttons ]

halfJoltage :: Joltage -> Joltage
halfJoltage joltage = [j `div` 2 | j <- joltage]

applyButton :: Joltage -> Button -> Joltage
applyButton joltage button = [if i `elem` button then j - 1 else j
                             | (i,j) <- zip [0..] joltage
                             ]

solve :: Joltage -> [Button] -> Int
solve joltage buttons = fst (go M.empty joltage)
  where
    parityMap = buildParityMap (length joltage) buttons
    -- note that memo is monotonic
    go memo jolts
      | all (== 0) jolts = (0, memo)
      | Just v <- M.lookup jolts memo = (v, memo) -- already memoized
      | otherwise =
          let p = parity jolts
              options = M.findWithDefault [] p parityMap -- button sequences that will give the right parity
              (vals, memo') = foldl' step ([], memo) options
              step (acc, m) bs =
                let j' = foldl applyButton jolts bs -- get updated jolts
                in if all (>= 0) j'
                   then
                     let (v, m') = go m (halfJoltage j') -- recurse on the half joltage
                     in ( (length bs + 2 * v) : acc, m' ) -- must add the buttons we pressed
                   else (acc, m) -- invalid joltage so don't include it
              ans = if null vals then 10^9 else minimum vals
              memo'' = M.insert jolts ans memo'
          in (ans, memo'')

part2 :: String -> IO Int
part2 s = do
  let machines = map processLine2 $ lines s
  return $ sum $ map (uncurry solve) machines

