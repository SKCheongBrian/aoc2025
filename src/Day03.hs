module Day03 (run) where

import Data.Function ((&))

run :: IO ()
run = do
	input <- readFile "inputs/day03.txt"
	answer1 <- part1 input
	answer2 <- part2 input
	putStrLn $ "Part1: " ++ show answer1
	putStrLn $ "Part2: " ++ show answer2

part1 s = do
  let solve s = maximum [ 10 * read [a] + read [b]
												| (i, a) <- zip [0 ..] s
												, (j, b) <- zip [0 ..] s
												, i < j
												] 

      splitted = lines s
      nums = map solve splitted

  print nums
  return (sum nums)

maxNumber :: Int -> String -> String
maxNumber k s = take k $ go [] (length s - k) s
  where
    go stack _ [] = reverse stack
    go stack remain (c:cs) =
			case stack of
				[] -> go [c] remain cs
				(t:ts) ->
					if t < c && remain > 0 then
						go ts (remain - 1) (c:cs)
					else
						go (c : stack) remain cs


part2 :: String -> IO Integer
part2 s = do
  let splitted = lines s
      solve bank = read (maxNumber 12 bank) :: Integer
      nums = map solve splitted

  print nums
  return (sum nums)