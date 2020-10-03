module Test.Forecast where

import Data.DateTime (Date, date)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Maybe (Maybe(..))

import Data.Teller.Forecast (TrendDescriptionStruct(..), forecast, generateTrendId)
import Data.Teller.GenTypes (TrendDescription(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, discard, pure, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Show

testTrends :: Array TrendDescriptionStruct
testTrends = [
  TrendDescriptionStruct {
    id: "trendA",
    merchant: "A",
    trend: MonthDayTrendDescription {
      dayOfMonth: 25,
      pricing: 100.0
    }
  },
  TrendDescriptionStruct {
    id: "trendB",
    merchant: "B",
    trend: LastWeekdayTrendDescription {
      weekday: 4,
      pricing: 100.0
    }
  },
  TrendDescriptionStruct {
    id: "trendC",
    merchant: "C",
    trend: SpecificWeekdayTrendDescription {
      weekday: 5,
      pricing: 100.0
    }
  },
  TrendDescriptionStruct {
    id: "trendD",
    merchant: "D",
    trend: WeekdayTrendDescription {
      weekdays: [3, 6, 5],
      pricing: [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
    }
  },
  TrendDescriptionStruct {
    id: "trendE",
    merchant: "E",
    trend: EveryWeekdayTrendDescription {
      pricing: [100.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
    }
  },
  TrendDescriptionStruct {
    id: "trendF",
    merchant: "F",
    trend: WeekendTrendDescription {
      pricing: [100.0, 30.0]
    }
  }
]

startDate :: Maybe Date
startDate = 
  case i of
    (Just inst) -> pure (date $ toDateTime $ inst)
    Nothing -> Nothing
  where
    i = instant $ Milliseconds 1603580400000.0 -- 24 oct 2020
  
endDate :: Maybe Date
endDate = 
  case i of
    (Just inst) -> pure (date $ toDateTime $ inst)
    Nothing -> Nothing
  where
    i = instant $ Milliseconds 1604188800000.0 -- 1 nov 2020

forecastSpec :: Spec Unit
forecastSpec =
  describe "Forecast" do
    describe "forecast" do
      it "returns the correct forecast" do
        res <- pure $ do
          s <- startDate
          e <- endDate
          pure $ forecast s e testTrends
        case res of
          (Just r) -> (show r) `shouldEqual` "[1603497600000.0[\"trendD\",\"trendF\"],1603584000000.0[\"trendA\",\"trendF\"],1603670400000.0[\"trendE\"],1603756800000.0[\"trendE\"],1603843200000.0[\"trendD\",\"trendE\"],1603929600000.0[\"trendB\",\"trendE\"],1604016000000.0[\"trendC\",\"trendD\",\"trendE\"],1604102400000.0[\"trendD\",\"trendF\"],1604188800000.0[\"trendF\"]]"
          Nothing -> false `shouldEqual` true
        
    describe "generateTrendId" do
      it "returns the correct id for MonthDayTrendDescription" do
        let res = generateTrendId (
            Tuple "myMerchant" (MonthDayTrendDescription {dayOfMonth : 1, pricing: 1.0})
        )
        res `shouldEqual` "MonthDay_1_myMerchant"
      it "returns the correct id for LastWeekdayTrendDescription" do
        let res = generateTrendId (
            Tuple "myMerchant" (LastWeekdayTrendDescription {weekday : 1, pricing: 1.0})
        )
        res `shouldEqual` "LastWeekday_1_myMerchant"
      it "returns the correct id for SpecificWeekdayTrendDescription" do
        let res = generateTrendId (
            Tuple "myMerchant" (SpecificWeekdayTrendDescription {weekday : 1, pricing: 1.0})
        )
        res `shouldEqual` "SpecificWeekday_1_myMerchant"
      it "returns the correct id for WeekdayTrendDescription" do
        let res = generateTrendId (
            Tuple "myMerchant" (WeekdayTrendDescription {weekdays : [1, 2, 5], pricing: [1.0, 1.0, 1.0]})
        )
        res `shouldEqual` "Weekday_[1,2,5]_myMerchant"
      it "returns the correct id for EveryWeekdayTrendDescription" do
        let res = generateTrendId (
            Tuple "myMerchant" (EveryWeekdayTrendDescription {pricing: [1.0]})
        )
        res `shouldEqual` "EveryWeekday_myMerchant"
      it "returns the correct id for WeekendTrendDescription" do
        let res = generateTrendId (
            Tuple "myMerchant" (WeekendTrendDescription {pricing: [1.0]})
        )
        res `shouldEqual` "Weekend_myMerchant"