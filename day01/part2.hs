applyTwist :: (Int, Int) -> (Char, Int) -> (Int, Int)
applyTwist (val, zeroes) (p, cnt) =
  let whole = cnt `div` 100
      partial = cnt `mod` 100
      delta = if p == 'L' then -partial else partial
      nextVal = val + delta
      crosses = if val /= 0 && nextVal `notElem` [1 .. 99] then 1 else 0
   in (nextVal `mod` 100, zeroes + whole + crosses)

password :: Int -> [String] -> Int
password val ops =
  let (_, zeroes) =
        foldl applyTwist (val, 0) $ map (\(x : xs) -> (x, read xs)) ops
   in zeroes

main :: IO ()
main = do
  inputs <- readFile "input.txt"
  print $ password 50 $ lines inputs
