safeIndex :: [a] -> Int -> Maybe a
safeIndex l i | i < 0 || i >= (length l) = Nothing
safeIndex l i = Just (l !! i)

surroundCount :: [[Char]] -> (Int, Int) -> Int
surroundCount g (x, y) =
  let offsets =
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
      neighbors = [(x + dx, y + dy) | (dx, dy) <- offsets]
   in length $ filter (== Just '@') $ map (\(nx, ny) -> safeIndex g ny >>= \row -> safeIndex row nx) neighbors

targetRolls :: [[Char]] -> [(Int, Int)]
targetRolls g =
  let allPos = [(x, y) | x <- [0 .. length g - 1], y <- [0 .. length (g !! x) - 1]]
      isTarget (x, y) = case safeIndex g y >>= \row -> safeIndex row x of
        Just '@' -> surroundCount g (x, y) < 4
        _ -> False
   in filter isTarget allPos

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex idx val li = let (before, after) = splitAt idx li in before ++ val : (drop 1 after)

takeRoll :: [[Char]] -> [(Int, Int)] -> [[Char]]
takeRoll grid [] = grid
takeRoll grid ((x, y) : ps) =
  let row = grid !! y
      newRow = replaceAtIndex x '.' row
      newGrid = replaceAtIndex y newRow grid
   in takeRoll newGrid ps

paperRolls :: [[Char]] -> Int -> Int
paperRolls grid total =
  let targetPos = targetRolls grid
      newGrid = takeRoll grid targetPos
   in if length targetPos == 0 then total else paperRolls newGrid (total + length targetPos)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ paperRolls (lines input) 0
