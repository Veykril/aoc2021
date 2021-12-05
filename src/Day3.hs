module Day3 where

import Data.Bits (Bits (shiftL, (.|.)))
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (transpose)

main :: IO ()
main = do
  input <- map (map $ (1 ==) . digitToInt) . lines <$> readFile "data/day3.txt"
  print $ part1 input
  print $ part2 input

-- this is cursed, all of it, but it works

part2 :: [[Bool]] -> Integer
part2 numbers = co2ScrubberRating numbers * oxygenGeneratorRating numbers

type Rating a = a -> a -> Bool

getRowFilter :: Rating Int -> [Bool] -> [[Bool]] -> [[Bool]]
getRowFilter rating row numbers = map snd $ filter fst $ zip filterList numbers
  where
    countOnes = length $ filter id row
    mostCommonBit = rating countOnes $ length row - countOnes
    filterList = map (== mostCommonBit) row

doRating :: Rating Int -> Int -> [[Bool]] -> Integer
doRating _ _ [bits] = binToInt bits
doRating rating idx numbers =
  doRating rating (idx + 1) $ getRowFilter rating (transpose numbers !! idx) numbers

co2ScrubberRating :: [[Bool]] -> Integer
co2ScrubberRating = doRating (<) 0

oxygenGeneratorRating :: [[Bool]] -> Integer
oxygenGeneratorRating = doRating (>=) 0

part1 :: [[Bool]] -> Integer
part1 numbers = binToInt gammaBits * binToInt epsilonBits
  where
    countOnes = map (length . filter id) $ transpose numbers
    countZeroes = map (length numbers -) countOnes
    epsilonBits = zipWith (>=) countOnes countZeroes
    gammaBits = map not epsilonBits

binToInt :: [Bool] -> Integer
binToInt = foldl' (\acc x -> shiftL acc 1 .|. toInteger (fromEnum x)) 0
