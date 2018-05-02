import Data.List (sort)

type StringWithOffset = (String, Int)
type SortedStrings = [String]
type StringOccurrency = (String, Int)

wordsOffsets :: StringWithOffset -> [String]
wordsOffsets (s, i)
  | s == "" = []
  | i == -1 + length s = [s]
  | s!!i == '\n' = [take i s] ++ ( wordsOffsets ( (drop (i + 1) s), 0) )
  | otherwise = wordsOffsets (s, i + 1)

wordsOffsets0 :: String -> [String]
wordsOffsets0 s = wordsOffsets (s, 0)

sortStrings :: [String] -> SortedStrings
sortStrings s = sort s

sortedCount :: (SortedStrings, String, Int) -> [StringOccurrency]
sortedCount0 :: SortedStrings -> [StringOccurrency]

sortedCount (sorted, current, count)
  | sorted == [] = if current /= "" then [(current, count)] else []
  | current == "" = sortedCount ( (drop 1 sorted), sorted!!0, 1)
  | current == sorted!!0 = sortedCount( (drop 1 sorted), current, (count + 1) )
  | otherwise = [(current, count)] ++ ( sortedCount ( ( drop 1 sorted), "", 0) )
sortedCount0 sorted = sortedCount (sorted, "", 0)

count = sortedCount0 . sortStrings . wordsOffsets0
