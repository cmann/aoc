import Data.Maybe
import Data.List
import Data.List.Split
import Text.Read

part1 ids ts = id * (best - ts)
  where
    times = map (\x -> x * (1 + quot ts x)) ids
    best = minimum times
    (Just i) = elemIndex best times
    id = ids !! i

part2 ((_,id):xs) = fst $ foldl' lcm (id,id) xs
  where
    lcm (ts,inc) (off,id) =
      case mod (ts+off) id of
        0 -> (ts,inc*id)
        _ -> lcm (ts+inc,inc) (off,id)

main = do
  input <- lines <$> getContents
  let parsed = map readMaybe . splitOn "," $ last input
  let ts = read $ head input
  let ids = catMaybes parsed
  print $ part1 ids ts
  let offsets = [(off,id) | (off,Just id) <- zip [0..] parsed]
  print $ part2 offsets
