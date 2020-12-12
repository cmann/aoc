import qualified Data.Array as Array

occupied (i,j) xss =
  adjacent
  where
    adjacent = [(i + x, j + y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x /= 0) || (y /= 0)]

main = do
  input <- lines <$> getContents
  let indexed = zip [0..] $ map (zip [0..]) input
  let width = length $ head input
  let height = length $ input
  let a = Array.array ((0,0),(height-1,width-1)) [((i,j), seat) | (i, row) <- indexed, (j, seat) <- row]
  print $ occupied (10,10) a
  -- print $ a !? (100,100)
