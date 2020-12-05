import Data.Map (Map, fromList, member, (!))
import Data.Char
import Text.Read
import Text.Parsec

passports = map fromList <$> sepEndBy passport newline
passport = many kv
kv = (,) <$> key <*> value
key = manyTill letter $ char ':'
value = manyTill anyChar end
end = newline <|> space

valid1 passport = member "byr" passport &&
                  member "iyr" passport &&
                  member "eyr" passport &&
                  member "hgt" passport &&
                  member "hcl" passport &&
                  member "ecl" passport &&
                  member "pid" passport

valid2 passport = valid1 passport &&
                  byr (passport ! "byr") &&
                  iyr (passport ! "iyr") &&
                  eyr (passport ! "eyr") &&
                  hgt (passport ! "hgt") &&
                  hcl (passport ! "hcl") &&
                  ecl (passport ! "ecl") &&
                  pid (passport ! "pid")

byr xs = 
  case readMaybe xs of
    Just n  -> n >= 1920 && n <= 2002
    Nothing -> False

iyr xs = 
  case readMaybe xs of
    Just n  -> n >= 2010 && n <= 2020
    Nothing -> False

eyr xs = 
  case readMaybe xs of
    Just n  -> n >= 2020 && n <= 2030
    Nothing -> False

hgt xs =
  case height of
    Just h    -> case units of
                   "cm"      -> h >= 150 && h <= 193
                   "in"      -> h >= 59 && h <= 76
                   otherwise -> False
    otherwise -> False
  where
    (value, units) = splitAt (length xs - 2) xs
    height = readMaybe value

hcl ('#':hex) =
  case length hex of
    6         -> all isHexDigit hex
    otherwise -> False
hcl _ = False

ecl xs = elem xs ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pid xs = length xs == 9 && all isDigit xs

main = do
  input <- getContents
  let (Right parsed) = parse passports "" input
  print . length $ filter valid1 parsed
  print . length $ filter valid2 parsed
