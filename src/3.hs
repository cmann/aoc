countTrees lines slope = countTrees' lines slope 0 0
countTrees' [] _ _ trees = trees
countTrees' (line:lines) (dx, dy) x trees = countTrees' lines' (dx, dy) x' trees'
  where
    x'     = (x + dx) `mod` (length line)
    lines' = drop (dy - 1) lines
    trees' = trees + if line !! x == '#' then 1 else 0

main = do
  lines <- lines <$> getContents
  print $ countTrees lines (3, 1)
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print . product $ map (countTrees lines) slopes
