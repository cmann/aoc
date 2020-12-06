import Data.List
import Data.List.Split

main = do
  groups <- splitOn "\n\n" <$> getContents
  print . sum $ map part1 groups
  let groups' = map (split (dropDelims . dropFinalBlank $ oneOf "\n")) groups
  print . sum $ map part2 groups'
  where
    part1 = length . nub . filter (/= '\n')
    part2 (xs:xss) = length $ filter (\x -> all (elem x) xss) xs
