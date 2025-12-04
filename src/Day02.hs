module Day02 (run) where

run :: IO ()
run = do
    input <- readFile "inputs/day02.txt"
    let 
      answer1 = part1 input
      answer2 = part2 input
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [(Int, Int)]
parse s = map intpair (split (==',') s)
  where
    -- intpair :: String -> (Int, Int)
    intpair x = 
      let (a, rest) = break (=='-') x
          b = drop 1 rest
      in (read a, read b)
    split p s = case dropWhile (\x -> p x || x == '\n') s of
      "" -> []
      s' -> w : split p s''
        where
          (w, s'') = break p s'

solve :: [(Int, Int)] -> Int
solve [] = 0
solve (x:xs) = case x of
  (lower, upper) -> sum (filter isvalid [lower .. upper]) + solve xs
    where
      isvalid :: Int -> Bool
      isvalid n = 
        let 
          s = show n 
          len = length s
          repeatPattern d =
              len `mod` d == 0
              && let 
                chunk = take d s
                times = len `div` d
              in times >= 2 && concat (replicate times chunk) == s
        in any repeatPattern [1 .. len `div` 2]
            

-- part1 :: String -> Int
part1 s = solve $ parse s

part2 :: String -> Int
part2 x = 0