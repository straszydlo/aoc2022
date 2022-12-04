module Lib
    ( someFunc
    ) where

import Data.Map(Map, (!))
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

type Item = Char
type Priority = Int
data Rucksack = Rucksack { one :: Set Item, two :: Set Item }

prioritize :: Rucksack -> Priority
prioritize rucksack = priorities ! (common rucksack)

rucksack :: [Item] -> Rucksack
rucksack items
  | even $ length items =
      let (one, two) = splitAt ((length items) `div` 2) items
      in Rucksack (Set.fromList one) (Set.fromList two)
  | otherwise = error $ "uneven number of items in rucksack: " ++ items

common :: Rucksack -> Item
common rucksack = case Set.null $ intersection of
  True -> error "no common items found"
  False -> head $ Set.toList intersection
  where intersection = Set.intersection (one rucksack) (two rucksack)


priorities :: Map Item Priority
priorities = Map.fromList $ lowercases ++ uppercases
  where lowercases = zip ['a'..'z'] [1..]
        uppercases = zip ['A'..'Z'] [27..]

someFunc :: String -> IO ()
someFunc filename =
  readFile filename >>= print . sum . map prioritize . map rucksack . lines

