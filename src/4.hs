import Data.Map (Map, fromList, member, (!))
import Data.Ix (inRange)
import Data.Maybe
import Data.Char
import Text.Read
import Text.Parsec

passports = map fromList <$> sepEndBy passport newline
passport = many kv
kv = (,) <$> key <*> value
key = manyTill letter $ char ':'
value = manyTill anyChar end
end = newline <|> space

valid1 passport = all (`member` passport) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

valid2 passport = valid1 passport &&
                  byr (passport ! "byr") &&
                  iyr (passport ! "iyr") &&
                  eyr (passport ! "eyr") &&
                  hgt (passport ! "hgt") &&
                  hcl (passport ! "hcl") &&
                  ecl (passport ! "ecl") &&
                  pid (passport ! "pid")

byr xs = maybe False (inRange (1920, 2002)) $ readMaybe xs

iyr xs = maybe False (inRange (2010, 2020)) $ readMaybe xs

eyr xs = maybe False (inRange (2020, 2030)) $ readMaybe xs

hgt xs =
  case listToMaybe $ reads xs of
    Just (h, "cm") -> inRange (150, 193) h
    Just (h, "in") -> inRange (59, 76) h
    otherwise -> False

hcl ('#':hex) = length hex == 6 && all isHexDigit hex
hcl _         = False

ecl xs = elem xs ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pid xs = length xs == 9 && all isDigit xs

main = do
  input <- getContents
  let (Right parsed) = parse passports "" input
  print . length $ filter valid1 parsed
  print . length $ filter valid2 parsed
