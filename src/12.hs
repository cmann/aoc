import Data.List

part1 ship@((x,y), rot) (a,v) =
  case a of
    'N' -> ((x,y+v), rot)
    'S' -> ((x,y-v), rot)
    'E' -> ((x+v,y), rot)
    'W' -> ((x-v,y), rot)
    'L' -> ((x,y), rot-v)
    'R' -> ((x,y), rot+v)
    'F' -> part1 ship (dir,v)
      where
        dir = ['E', 'S', 'W', 'N'] !! div norm 90
        norm = mod ((mod rot 360) + 360) 360

part2 (ship@(xs,ys), wp@(xw,yw)) (a,v) =
  case a of
    'N' -> (ship, (xw,yw+v))
    'S' -> (ship, (xw,yw-v))
    'E' -> (ship, (xw+v,yw))
    'W' -> (ship, (xw-v,yw))
    'L' -> (ship, rotate wp (360-v))
    'R' -> (ship, rotate wp v)
    'F' -> ((xs+v*xw,ys+v*yw), wp)
  where
    rotate p 0 = p
    rotate (x,y) deg = rotate (y,-x) (deg-90)

main = do
  input <- map (\(x:xs) -> (x, read xs :: Int)) . lines <$> getContents
  let ((x,y), _) = foldl' part1 ((0,0), 0) input
  print $ abs x + abs y
  let ((x,y), _) = foldl' part2 ((0,0), (10,1)) input
  print $ abs x + abs y
