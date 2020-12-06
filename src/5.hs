import Data.List
import Numeric (readInt)

toBinary 'F' = 0
toBinary 'L' = 0
toBinary 'B' = 1
toBinary 'R' = 1

findSeat (lo:hi:xs)
  | hi - lo == 2 = lo + 1
  | otherwise    = findSeat (hi:xs)
  
main = do
  lines <- lines <$> getContents
  let ids = map (fst . head . readInt 2 (`elem` "FLBR") toBinary) lines
  print $ maximum ids
  print . findSeat $ sort ids
