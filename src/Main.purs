module Main where

import Data.Date
import Effect.Now
import Prelude

import Data.List (List)
import Effect (Effect)
import Effect.Console (log)
import DateHelpers (getStartOfMonth, getXDaysPrior, maybeDateToString)

main :: Effect Unit
main = do
  now <- nowDate
  analysisEndDate <- pure $ getStartOfMonth now
  analysisStartDate <- pure $ getXDaysPrior analysisEndDate $ -95
  log $ maybeDateToString analysisEndDate
  log $ maybeDateToString analysisStartDate
    
