applyTwist :: (Int, Int) -> (Char, Int) -> (Int, Int)
applyTwist (val, zeroes) (p, cnt) =
  let delta = if p == 'L' then -cnt else cnt
      nextVal = (val + delta) `mod` 100
   in if nextVal == 0
        then (nextVal, zeroes + 1)
        else (nextVal, zeroes)

password :: Int -> [String] -> Int
password val ops =
  let (_, zeroes) =
        foldl applyTwist (val, 0) $ map (\(x : xs) -> (x, read xs)) ops
   in zeroes

main :: IO ()
main = do
  inputs <- readFile "input.txt"
  print $ password 50 $ lines inputs
