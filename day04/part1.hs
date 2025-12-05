safeIndex :: [a] -> Int -> Maybe a
safeIndex l i | i < 0 || i >= (length l) = Nothing
safeIndex l i = Just (l !! i)

surroundCount :: [[Char]] -> (Int, Int) -> Int
surroundCount grid (x, y) =
  let offsets =
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
      neighbors = [(x + dx, y + dy) | (dx, dy) <- offsets]
   in length $ filter (== Just '@') $ map (\(nx, ny) -> safeIndex grid ny >>= \row -> safeIndex row nx) neighbors

paperRolls :: [[Char]] -> Int
paperRolls grid =
  let allPos = [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (grid !! x) - 1]]
      isTarget (x, y) = case safeIndex grid y >>= \row -> safeIndex row x of
        Just '@' -> surroundCount grid (x, y) < 4
        _ -> False
   in length $ filter isTarget allPos

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ paperRolls $ lines input
