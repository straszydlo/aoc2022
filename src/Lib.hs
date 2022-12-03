module Lib
    ( someFunc
    ) where

import Control.Monad.State
import Data.Maybe
import Text.Read(readMaybe)

type Result = Int
type Accumulation = Int

data Step = UpdateMax | Accumulate Int

parseStep :: String -> Step
parseStep str = maybe UpdateMax Accumulate $ readMaybe str

solve :: [Step] -> Result
solve steps =
  let (acc, res) = runState (solveS steps) 0
  in  max acc res

solveS :: [Step] -> State Result Accumulation
solveS steps = foldM accumulateOrUpdate 0 steps

accumulateOrUpdate :: Accumulation -> Step -> State Result Accumulation
accumulateOrUpdate acc step = case step of
  UpdateMax -> do
    modify $ max acc
    return 0
  Accumulate value ->
    return $ acc + value

someFunc :: String -> IO ()
someFunc filename =
  readFile filename >>= print . solve . map parseStep . lines

