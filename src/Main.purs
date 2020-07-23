module Main where

import Prelude

import DateHelpers (getStartOfMonth, getXDaysPrior, maybeDateToString)
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDate)

main :: Effect Unit
main = do
  now <- nowDate
  analysisEndDate <- pure $ getStartOfMonth now
  analysisStartDate <- pure $ getXDaysPrior analysisEndDate $ -95
  log $ maybeDateToString analysisEndDate
  log $ maybeDateToString analysisStartDate
    
