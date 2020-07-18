module Test.Main where

import Prelude

import DateHelpers (getStartOfMonth)
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Class.Console (log)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Now
import Data.Enum
import Data.Date
import Data.Maybe

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Date Helpers" do
    describe "Get Start Of Month" do
      it "Returns the first of the month" do
        testYear <- pure $ (fromMaybe bottom $ toEnum 1994)
        testDay <- pure $ (fromMaybe bottom $ toEnum 4)
        testMonth <- pure July
        date <- pure $ canonicalDate testYear testMonth testDay
        firstDay <- pure $ (fromMaybe bottom $ toEnum 1)
        result <- pure $ getStartOfMonth date
        case result of
          Nothing -> false `shouldEqual` true
          (Just d) -> (day d) `shouldEqual` firstDay
        
