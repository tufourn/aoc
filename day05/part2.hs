mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges [a] = [a]
mergeRanges (p : ps) =
  let isOverlapping (l1, r1) (l2, r2) = (l1 <= r2 && r1 >= l2) || (l2 <= l1 && r2 >= l1)
      overlaps = filter (isOverlapping p) ps
      nonOverlaps = filter (not . isOverlapping p) ps
      mergedOverlaps = foldl (\(l1, r1) (l2, r2) -> (min l1 l2, max r1 r2)) p overlaps
   in if not (null overlaps)
        then mergeRanges $ mergedOverlaps : nonOverlaps
        else p : mergeRanges ps

cafeteria :: [String] -> Integer
cafeteria inputs =
  let (ranges, _ : _ids) = break (== "") inputs
      parseRange r = let (ls, _ : rs) = break (== '-') r in (read ls, read rs)
      mergedRanges = mergeRanges $ map parseRange ranges
   in sum $ map (\(a, b) -> b - a + 1) mergedRanges

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ cafeteria $ lines input
