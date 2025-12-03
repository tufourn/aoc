joltage :: Int -> [Int] -> Int
joltage 0 xs = maximum xs
joltage n xs =
  let l = maximum $ take (length xs - n) xs
   in l * (10 ^ n) + joltage (n - 1) (drop 1 (dropWhile (/= l) xs))

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ map (joltage 11) $ map (map (\c -> read [c] :: Int)) (lines input)
