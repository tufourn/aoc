import Data.Set (Set, fromList)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s'' where (w, s'') = break p s'

processId :: String -> Int
processId p =
  let (ls, _ : rs) = break (== '-') p
      (l, r) = (read ls, read rs)
      ids =
        [ id
        | x <- [1 .. ceiling $ sqrt $ fromIntegral r],
          n <- [2 .. (length $ show r) `div` (length $ show x)],
          let id = read $ concat $ replicate n (show x),
          id >= l && id <= r
        ]
      uids = Data.Set.fromList ids
   in sum uids

main :: IO ()
main = do
  inputs <- readFile "input.txt"
  let inputs' = wordsWhen (== ',') $ (reverse . dropWhile (== '\n') . reverse) inputs
  print $ sum $ map processId inputs'
