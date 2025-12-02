wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s'' where (w, s'') = break p s'

processId :: String -> Int
processId p =
  let (ls, _ : rs) = break (== '-') p
      (l, r) = (read ls, read rs)
   in sum
        [ id
        | x <- [1 .. ceiling $ sqrt $ fromIntegral r],
          let id = read $ (show x ++ show x),
          id >= l && id <= r
        ]

main :: IO ()
main = do
  inputs <- readFile "input.txt"
  let inputs' = wordsWhen (== ',') $ (reverse . dropWhile (== '\n') . reverse) inputs
  print $ sum $ map processId inputs'
