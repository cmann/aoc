{-# LANGUAGE BangPatterns #-}
import Data.List.Split
import qualified Data.IntMap.Strict as IM

-- part1 _ 2020 x = x
part1 _ 30000000 x = x
part1 hist turn !x =
  part1 (IM.insert x turn hist) (turn+1) $ case IM.lookup x hist of
                                             Just prev -> turn-prev
                                             Nothing   -> 0

main = do
  input <- map read . splitOn "," <$> getContents :: IO [Int]
  let starting = IM.fromList $ zip input [1..]
  print $ part1 starting (length input) (last input)
