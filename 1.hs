import Data.List

main = do
  nums <- map read . lines <$> getContents
  print $ head [x * y | (x:ys) <- tails nums, y <- ys, x + y == 2020]
  print $ head [x * y * z | (x:ys) <- tails nums, (y:zs) <- tails ys, z <- zs, x + y + z == 2020]
