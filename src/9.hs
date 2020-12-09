import Data.List

part1 nums =
  case [n | (x:ys) <- tails xs, y <- ys, x + y == n] of
    [] -> n
    _  -> part1 $ tail nums
  where
    n = nums !! 25
    xs = take 25 nums

part2 n (x:xs) =
  case findRange n (x:xs) 0 [] of
    Just range -> minimum range + maximum range
    Nothing    -> part2 n xs

findRange _ [] _ _ = Nothing
findRange n (x:xs) sum range
  | sum + x == n = Just (x:range)
  | otherwise    = findRange n xs (sum + x) (x:range)

main = do
  input <- lines <$> getContents
  let nums = map (read) input
  let n = part1 nums
  print n
  print $ part2 n nums
