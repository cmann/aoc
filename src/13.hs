import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Text.Read

part1 ids ts = fst $ minimumBy (comparing snd) deltas
  where deltas = [(id*delta,delta) | id <- ids, let delta = id - mod ts id]

part2 ((id,off):xs) = fst $ foldl' lcm (id,id) xs
  where
    lcm (ts,inc) (id,off) =
      case mod (ts+off) id of
        0 -> (ts,inc*id)
        _ -> lcm (ts+inc,inc) (id,off)

main = do
  input <- lines <$> getContents
  let parsed = map readMaybe . splitOn "," $ last input
  let ts = read $ head input
  let ids = catMaybes parsed
  print $ part1 ids ts
  let offsets = [(id,off) | (off,Just id) <- zip [0..] parsed]
  print $ part2 offsets
