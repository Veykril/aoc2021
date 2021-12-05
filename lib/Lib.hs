module Lib (readFileLinesParsedAs, windows) where

readFileLinesParsedAs :: Read a => FilePath -> IO [a]
readFileLinesParsedAs path = map read . lines <$> readFile path

windows :: Int -> [a] -> [[a]]
windows len list =
  case list of
    x : xs | len <= length list -> take len list : windows len xs
    _ -> []
