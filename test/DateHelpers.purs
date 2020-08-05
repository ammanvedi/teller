module Test.Date where

import Data.Date
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Teller.DateHelpers (getStartOfMonth, getXDaysPrior)
import Prelude (Unit, bind, bottom, discard, negate, pure, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


dateSpec :: Spec Unit
dateSpec =
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