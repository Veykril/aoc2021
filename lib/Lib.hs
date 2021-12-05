module Lib (readFileLinesParsedAs, readFileLinesMapped, windows) where

readFileLinesParsedAs :: Read a => FilePath -> IO [a]
readFileLinesParsedAs path = readFileLinesMapped path read

readFileLinesMapped :: FilePath -> (String -> a) -> IO [a]
readFileLinesMapped path f = map f . lines <$> readFile path

windows :: Int -> [a] -> [[a]]
windows len list =
  case list of
    x : xs | len <= length list -> take len list : windows len xs
    _ -> []
