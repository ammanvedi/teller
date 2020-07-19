module Test.Main where

import Data.Date
import Data.Enum
import Data.Maybe
import Effect.Now
import Prelude

import DateHelpers (getStartOfMonth, getXDaysPrior, maybeDateToString)
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Class.Console (log)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Date Helpers" do
    describe "getStartOfMonth" do
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
    describe "getXDaysPrior" do
      it "returns x days prior" do
        testYear <- pure $ (fromMaybe bottom $ toEnum 1994)
        testDay <- pure $ (fromMaybe bottom $ toEnum 4)
        testMonth <- pure July
        date <- pure $ canonicalDate testYear testMonth testDay
        targetDay <- pure $ (fromMaybe bottom $ toEnum 14)
        targetYear :: Year <- pure $ (fromMaybe bottom $ toEnum 1994)
        result <- pure $ getXDaysPrior (Just date) (-20)
        case result of
          Nothing -> false `shouldEqual` true
          (Just d) -> do
              (day d) `shouldEqual` targetDay
              (month d) `shouldEqual` June
              (year d) `shouldEqual` targetYear