{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

type Score = Int
data Game = Game { theirMove :: Move, ourMove :: Move }
data Move = Rock | Paper | Scissors deriving Eq

class Points a where
  points :: a -> Int

instance Points Move where
  points Rock = 1
  points Paper = 2
  points Scissors = 3

instance Points Game where
  points (Game x y)
    | x == y = 3
    | x == Rock && y == Paper = 6
    | x == Paper && y == Scissors = 6
    | x == Scissors && y == Rock = 6
    | otherwise = 0

score :: [String] -> Score
score = sum . map evaluateGame . map parseGame

--this should handle errors gracefully, but we insanely assume sane input
parseGame :: String -> Game
parseGame [x, ' ', y] = Game { theirMove = parseMove x, ourMove = parseMove y }
parseGame other = error $ "invalid input: " ++ other

--this should handle errors gracefully, but we insanely assume sane input
parseMove :: Char -> Move
parseMove = \case
  'A' -> Rock
  'X' -> Rock
  'B' -> Paper
  'Y' -> Paper
  'C' -> Scissors
  'Z' -> Scissors
  other   -> error $ "invalid move: " ++ [other]

evaluateGame :: Game -> Score
evaluateGame game = points game + points (ourMove game)

someFunc :: String -> IO ()
someFunc filename =
  readFile filename >>= (print . score . lines)

