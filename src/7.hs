import Text.Parsec

line = (,) <$> (bag <* string " contain ") <*> (contents <* char '.')
contents = sepBy (string ", ") (many digit <*> bag)
bag = (++) <$> many lower <*  string " " <*> many lower <* space <* many lower

main = do
  input <- lines <$> getContents
  print $ map (parse line "") input
