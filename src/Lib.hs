module Lib where

import Control.Monad.State
import Data.List(sort)
import Data.Maybe
import Text.Read(readMaybe)

newtype Result = Result { highest :: [Int] } deriving Show
type Accumulation = Int

data Step = UpdateMax | Accumulate Int deriving Show

parseStep :: String -> Step
parseStep str = maybe UpdateMax Accumulate $ readMaybe str

solve :: [Step] -> Result
solve steps =
  let (acc, res) = runState (solveS update steps) (Result [])
  in  update acc res
  where update acc result = Result . take 3 . reverse . sort $ acc : (highest result)

solveS :: (Accumulation -> Result -> Result) -> [Step] -> State Result Accumulation
solveS updater steps = foldM (accumulateOrUpdate updater) 0 steps

accumulateOrUpdate :: (Accumulation -> Result -> Result) -> Accumulation -> Step -> State Result Accumulation
accumulateOrUpdate updater acc step = case step of
  UpdateMax -> do
    modify $ updater acc
    return 0
  Accumulate value ->
    return $ acc + value

someFunc :: String -> IO ()
someFunc filename =
  readFile filename >>= print . sum . highest . solve . map parseStep . lines

