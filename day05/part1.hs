cafeteria :: [String] -> Int
cafeteria inputs =
  let (ranges, _ : ids) = break (== "") inputs
      ranges' = map (\s -> let (ls, _ : rs) = break (== '-') s in (read ls :: Int, read rs :: Int)) ranges
      ids' = map read ids
      inRange n (x, y) = (n >= x && n <= y)
   in length [id | id <- ids', any (inRange id) ranges']

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ cafeteria $ lines input
