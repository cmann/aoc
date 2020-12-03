{-# LANGUAGE RecordWildCards #-}

import Text.Parsec

data Entry = Entry { min :: Int, max :: Int, letter :: Char, password :: String }

entries =
  many $ Entry <$>
  ((read <$> many1 digit) <* char '-') <*>
  ((read <$> many1 digit) <* space) <*>
  (anyChar <* string ": ") <*>
  manyTill anyChar newline

isValid1 Entry {..} =
  count >= min && count <= max
  where count = length $ filter (== letter) password

isValid2 Entry {..} =
  (password !! (min-1) == letter) /= (password !! (max-1) == letter)

main = do
  input <- getContents
  let (Right xs) = parse entries "" input
  print . length $ filter isValid1 xs
  print . length $ filter isValid2 xs
