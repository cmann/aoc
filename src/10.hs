import Data.List

part1 (x:[]) (i, j) = i * (j + 1)
part1 (x:y:xs) (i, j)
  | y - x == 1 = part1 (y:xs) (i + 1, j)
  | y - x == 3 = part1 (y:xs) (i, j + 1)

part2 (x:xs) memo =
  case xs of
    [] -> i
    _  -> part2 xs ((x, i):memo)
  where
    i = sum . map snd $ takeWhile (\(y, _) -> x - y <= 3) memo
  
main = do
  input <- sort . map read . lines <$> getContents
  print $ part1 (0:input) (0, 0)
  print $ part2 input [(0, 1)]
