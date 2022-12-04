module Lib
    ( someFunc
    ) where

import Data.Set(Set, isSubsetOf)
import qualified Data.Set as Set

type Range = Set Int

total :: [(Range, Range)] -> Int
total = count (\(r1, r2) -> not . null $ Set.intersection r1 r2)

eitherContainsOther :: Range -> Range -> Bool
eitherContainsOther r1 r2 = r1 `isSubsetOf` r2 || r2 `isSubsetOf` r1

count :: (a -> Bool) -> [a] -> Int
count pred = foldl (\acc elem -> if pred elem then acc + 1 else acc) 0

parseRanges :: String -> (Range, Range)
parseRanges str = (parseRange left, parseRange right)
  where (left, right) = split ',' str
        parseRange str =
          let (beginning, end) = split '-' str
          in  Set.fromAscList [(read beginning :: Int)..(read end :: Int)]

split :: Eq a => a -> [a] -> ([a], [a])
split delim list = (takeWhile (/= delim) list, drop 1 . dropWhile (/= delim) $ list)

someFunc :: String -> IO ()
someFunc filename =
  readFile filename >>= print . total . map parseRanges . lines

