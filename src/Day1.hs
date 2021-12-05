module Day1 where

import Lib (readFileLinesParsedAs, windows)
import Util (last2)

main :: IO ()
main = do
  input <- readFileLinesParsedAs "data/day1.txt"
  print $ part1 input
  print $ part2 input

part1 :: [Int] -> Int
part1 input = length $ filter (uncurry (<)) $ map last2 $ windows 2 input

part2 :: [Int] -> Int
part2 input = part1 $ map sum $ windows 3 input
