module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Transaction (getStartOfMonth, maybeDateToString)
import Data.List (List)
import Data.Date
import Effect.Now

main :: Effect Unit
main = do
  now <- nowDate
  monthStart <- pure $ getStartOfMonth now
  dateString <- pure $ maybeDateToString monthStart
  log dateString
    
