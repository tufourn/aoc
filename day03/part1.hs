joltage :: (Int, Int) -> String -> Int
joltage (t, d) [] = read (show t ++ show d)
joltage (t, d) (c : cs) =
  let x = read [c] :: Int
      (t', d')
        | x > t && not (null cs) = (x, read $ [cs !! 0] :: Int)
        | x > t || x > d = (t, x)
        | otherwise = (t, d)
   in joltage (t', d') cs

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ map (joltage (0, 0)) $ lines input
