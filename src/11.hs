import Data.Maybe
import Data.Array
import Data.Ix

directions = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

simulate xss update =
  if xss == next then
    length $ filter (== '#') (elems xss)
  else
    simulate next update
  where
    next = listArray (bounds xss) . map (update xss) $ range (bounds xss)

part1 xss pos@(x,y) =
  case xss ! pos of
    'L' | occ == 0  -> '#'
    '#' | occ >= 4  -> 'L'
    x               -> x
  where
    occ = occupied xss adj
    adj = filter (inRange $ bounds xss) $ map (\(dx,dy) -> (x+dx,y+dy)) directions

part2 adj xss pos =
  case xss ! pos of
    'L' | occ == 0  -> '#'
    '#' | occ >= 5  -> 'L'
    x               -> x
  where
    occ = occupied xss $ adj ! pos

occupied xss adj = length . filter (== '#') $ map (xss !) adj

visibility xss = listArray (bounds xss) . map visible $ range (bounds xss)
  where visible pos = catMaybes $ map (nearest xss pos) directions

nearest xss (x,y) dir@(dx,dy)
  | inRange (bounds xss) pos =
    case xss ! pos of
      'L' -> Just pos
      _   -> nearest xss pos dir
  | otherwise = Nothing
  where
    pos = (x+dx,y+dy)

main = do
  input <- listArray((0,0), (98,91)) . filter (/= '\n') <$> getContents
  print $ simulate input part1
  print $ simulate input (part2 $ visibility input)
