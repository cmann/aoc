{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Maybe
import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

data State = State { pc   :: Int
                   , acc  :: Int
                   , hist :: Set Int
                   } deriving (Show)

newState = State 0 0 Set.empty

parseInst inst = (op, arg)
  where
    xs = words inst
    op = xs !! 0
    arg = read . dropWhile (== '+') $ xs !! 1

run prg state@State{..}
  | Set.member pc hist = Left state
  | pc >= Seq.length prg = Right state
  | otherwise =
      run prg $ case op of
                  "nop" -> State (pc + 1)   acc         hist'
                  "acc" -> State (pc + 1)   (acc + arg) hist'
                  "jmp" -> State (pc + arg) acc         hist'
                where
                  (op, arg) = Seq.index prg pc
                  hist' = Set.insert pc hist

swapOps prg pc =
  case op of
    "nop"     -> Just $ Seq.update pc ("jmp", arg) prg
    "jmp"     -> Just $ Seq.update pc ("nop", arg) prg
    otherwise -> Nothing
  where
    (op, arg) = Seq.index prg pc

runFixed prg = head . filter isRight $ map (`run` newState) swapped
  where
    Left State{hist} = run prg newState
    swapped = catMaybes . map (swapOps prg) $ Set.toList hist

main = do
  input <- lines <$> getContents
  let prg = Seq.fromList $ map parseInst input
  let (Left part1) = run prg newState
  print $ acc part1
  let (Right part2) = runFixed prg
  print $ acc part2
