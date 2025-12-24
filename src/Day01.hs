module Day01 (run) where

run :: IO ()
run = do
    input <- readFile "inputs/day01.txt"
    let answer1 = part1 input
        answer2 = part2 input
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Instr = LeftInstr Int | RightInstr Int
  deriving Show

parseInput :: String -> [Instr]
parseInput s = parseInput' $ lines s
  where
    parseInput' [] = []
    parseInput' (x:xs) = case x of
      ('L':n) -> LeftInstr (read n) : parseInput' xs
      ('R':n) -> RightInstr (read n) : parseInput' xs

inter :: [Instr] -> Int
inter = inter' 50 0
  where
    inter' _ c [] = c
    inter' n c (x:xs') = case x of
      (LeftInstr j)  -> case (n - j) `mod` 100 of
        0 -> inter' ((n - j) `mod` 100) (c + 1) xs'
        _ -> inter' ((n - j) `mod` 100) c xs'
      (RightInstr j) -> case (n + j) `mod` 100 of
        0 -> inter' ((n + j) `mod` 100) (c + 1) xs'
        _ -> inter' ((n + j) `mod` 100) c xs'

inter2 :: [Instr] -> Int
inter2 xs = inter' 50 0 xs
  where
    inter' _ c [] = c
    inter' n c (x:xs) = 
      let delta = case x of
            LeftInstr j -> -j
            RightInstr j -> j
          end = (n + delta) `mod` 100
          cycles = abs delta `div` 100
      in 
        if n == 0 then
          inter' end (c + cycles) xs
        else if end == 0 || (n > end && delta > 0) || (n < end && delta < 0) then
          inter' end (c + cycles + 1) xs
        else
          inter' end (c + cycles) xs

part1 :: String -> Int
part1 s = inter $ parseInput s

part2 :: String -> Int
part2 x = inter2 $ parseInput x
