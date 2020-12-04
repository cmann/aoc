import Data.Map (Map, fromList, member)
import Text.Parsec

data Passport = P { byr :: String
                  , iyr :: String
                  , eyr :: String
                  , hgt :: String
                  , hcl :: String
                  , ecl :: String
                  , pid :: String
                  , cid :: Maybe String
                  }

passports :: Parsec String () [Map String String]
passports = map fromList <$> sepEndBy passport newline
passport = many kv
kv = (,) <$> key <*> value
key = manyTill letter $ char ':'
value = manyTill anyChar end
end = newline <|> space

valid passport = member "byr" passport &&
                 member "iyr" passport &&
                 member "eyr" passport &&
                 member "hgt" passport &&
                 member "hcl" passport &&
                 member "ecl" passport &&
                 member "pid" passport

main = do
  input <- getContents
  let (Right parsed) = parse passports "" input
  print . length $ filter valid parsed
