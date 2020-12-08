import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read
import Text.Parsec

rules = Map.fromList <$> sepEndBy rule newline
rule = (,) <$> (bag <* string " contain ") <*> (contents <* char '.')
contents = [] <$ string "no other bags" <|> sepBy ((,) <$> many digit <* space <*> bag) (string ", ")
-- num = read <$> many digit :: Int
bag = (++) <$> many lower <* string " " <*> many lower <* space <* many lower

canHold rules color =
  colors ++ (concat $ map (canHold rules) colors)
  where
    colors = Map.keys $ Map.filter (\xs -> elem color $ map snd xs) rules

holds rules color =
  -- total + (sum . map (holds rules . snd) $ rule)
  sum $ map (\(i, j) -> i * j) foo
  where
    foo = map (\(i, c) -> (read i, holds rules c)) rule
    -- total = sum . map (read . fst) $ rule
    rule = rules ! color

main = do
  input <- getContents
  let (Right r) = parse rules "" input
  print . length . Set.fromList $ canHold r "shinygold"
  print $ holds r "shinygold"
