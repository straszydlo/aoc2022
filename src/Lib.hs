module Lib
    ( someFunc
    ) where

import Control.Monad.State
import Data.Map(Map, (!))
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

type Item = Char
type Priority = Int
newtype Rucksack = Rucksack (Set Item)

data AccumulateUpToTwo = None | One Rucksack | Two Rucksack Rucksack

rucksack :: [Item] -> Rucksack
rucksack = Rucksack . Set.fromList

common :: [Rucksack] -> Item
common [] = error "No rucksacks given"
common ((Rucksack r):rest) = head . Set.toList $ foldl (\acc (Rucksack x) -> Set.intersection x acc) r rest 


priorities :: Map Item Priority
priorities = Map.fromList $ lowercases ++ uppercases
  where lowercases = zip ['a'..'z'] [1..]
        uppercases = zip ['A'..'Z'] [27..]

solveS :: [Rucksack] -> State Priority ()
solveS = void . foldM accumulate None

accumulate :: AccumulateUpToTwo -> Rucksack -> State Priority AccumulateUpToTwo
accumulate None r = return $ One r
accumulate (One r1) r2 = return $ Two r1 r2
accumulate (Two r1 r2) r3 = modify ((+) (priorities ! (common [r1, r2, r3]))) >> return None

solve :: [Rucksack] -> Priority
solve rucksacks = execState (solveS rucksacks) 0

someFunc :: String -> IO ()
someFunc filename =
  readFile filename >>= print . solve . map rucksack . lines

