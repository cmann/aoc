import Data.List

partition [] (i, j) = i
partition (x:xs) (i, j)
  | x `elem` ['F', 'L'] = partition xs (i, k)
  | x `elem` ['B', 'R'] = partition xs (k + 1, j)
  where k = (i + j) `div` 2

findSeat (lo:hi:xs)
  | hi - lo == 2 = lo + 1
  | otherwise = findSeat (hi:xs)
  
main = do
  lines <- lines <$> getContents
  let ids = map (\(row, col) -> (partition row (0, 127)) * 8 + (binary col (0, 7))) $ map (splitAt 7) lines
  print $ maximum ids
  print . findSeat $ sort ids
