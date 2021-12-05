module Day2 where

import Data.Bifunctor (second)
import Data.Foldable (Foldable (foldl', toList))
import Lib (readFileLinesMapped)
import Util (foldl1', last2)

main :: IO ()
main = do
  input <- readFileLinesMapped "data/day2.txt" $ second read . last2 . words
  print $ part1 input
  print $ part2 input

part1 :: [(String, Int)] -> Int
part1 commands =
  uncurry (*) $ foldl' execute (0, 0) commands
  where
    execute (pos, depth) command =
      case command of
        ("forward", d) -> (pos + d, depth)
        ("down", d) -> (pos, depth + d)
        ("up", d) -> (pos, depth - d)
        _ -> (pos, depth)

part2 :: [(String, Int)] -> Int
part2 commands = (\(a, b, _) -> a * b) $ foldl' execute (0, 0, 0) commands
  where
    execute (pos, depth, aim) command =
      case command of
        ("forward", d) -> (pos + d, depth + d * aim, aim)
        ("down", d) -> (pos, depth, aim + d)
        ("up", d) -> (pos, depth, aim - d)
        _ -> (pos, depth, aim)
