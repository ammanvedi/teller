module Test.Price where

import Data.Date.Component (Weekday(..))
import Data.Teller.GenTypes (TrendDescription(..))
import Data.Teller.Price (addPricingToTrend, getLastAmount, getPricingForWeekdays, intArrayToWeekdayArray)
import Data.Teller.Transaction (TransactionRec(..))
import Prelude (Unit, discard, negate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testTransactions :: Array TransactionRec
testTransactions = [
    TransactionRec ({
            accountId: "acc",
            timestamp: 1595721600000.0, -- 26 jul 2020 sun
            amount: 1.0,
            merchantName: "TFL",
            reference: "TFL X"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1595808000000.0, -- 27 jul 2020 mon
            amount: 3.0,
            merchantName: "TFL",
            reference: "BAR XY"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1596758400000.0, -- 7 aug 2020 fri
            amount: 101.0,
            merchantName: "TFL",
            reference: "BAR XY"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1596931200000.0, -- 9 aug 2020 sun
            amount: 10.0,
            merchantName: "TFL",
            reference: "BAR XY"
        })
]

priceSpec :: Spec Unit
priceSpec =
    describe "Price" do 
        describe "getLastAmount" do
            it "returns 0 for an empty array" do
                let res = getLastAmount []
                res `shouldEqual` 0.0
            it "returns correct amount" do
                let res = getLastAmount testTransactions
                res `shouldEqual` 10.0
        describe "intArrayToWeekdayArray" do
            it "returns empty array when given empty array" do
                let res = intArrayToWeekdayArray []
                res `shouldEqual`[]
            it "sets defaults for bad ints" do
                let res = intArrayToWeekdayArray [100, 9, 1, 5, -1]
                res `shouldEqual` [Sunday,Sunday,Monday,Friday,Monday]
            it "return correctly for good data" do
                let res = intArrayToWeekdayArray [1, 2, 3, 4, 5, 6, 7]
                res `shouldEqual` [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
        describe "getPricingForWeekdays" do
            it "returns correct pricing for a set of weekdays" do
                let res = getPricingForWeekdays [Sunday, Monday, Thursday] testTransactions
                res `shouldEqual` [10.0, 3.0, 0.0]
        describe "addPricingToTrend" do
            it "returns last transaction value for MonthDayTrendDescription" do
                let trend = MonthDayTrendDescription {dayOfMonth: 26, pricing: 0.0}
                let res = addPricingToTrend testTransactions trend
                case res of
                    (MonthDayTrendDescription desc) -> desc.pricing `shouldEqual` 10.0
                    otherwise -> false `shouldEqual` true
            it "returns last transaction value for LastWeekdayTrendDescription" do
                let trend = LastWeekdayTrendDescription {weekday: 1, pricing: 0.0}
                let res = addPricingToTrend testTransactions trend
                case res of
                    (LastWeekdayTrendDescription desc) -> desc.pricing `shouldEqual` 10.0
                    otherwise -> false `shouldEqual` true
            it "returns last transaction value for SpecificWeekdayTrendDescription" do
                let trend = SpecificWeekdayTrendDescription {weekday: 1, pricing: 0.0}
                let res = addPricingToTrend testTransactions trend
                case res of
                    (SpecificWeekdayTrendDescription desc) -> desc.pricing `shouldEqual` 10.0
                    otherwise -> false `shouldEqual` true
            it "returns correct pricing for each weekday for WeekdayTrendDescription" do
                let trend = WeekdayTrendDescription {weekdays: [7, 1], pricing: [0.0]}
                let res = addPricingToTrend testTransactions trend
                case res of
                    (WeekdayTrendDescription desc) -> desc.pricing `shouldEqual` [10.0, 3.0]
                    otherwise -> false `shouldEqual` true
            it "returns correct pricing for each weekday for EveryWeekdayTrendDescription" do
                let trend = EveryWeekdayTrendDescription {pricing: [0.0]}
                let res = addPricingToTrend testTransactions trend
                case res of
                    (EveryWeekdayTrendDescription desc) -> desc.pricing `shouldEqual` [3.0, 0.0, 0.0, 0.0, 101.0]
                    otherwise -> false `shouldEqual` true
            it "returns correct pricing for each weekday for WeekendTrendDescription" do
                let trend = WeekendTrendDescription {pricing: [0.0]}
                let res = addPricingToTrend testTransactions trend
                case res of
                    (WeekendTrendDescription desc) -> desc.pricing `shouldEqual` [0.0, 10.0]
                    otherwise -> false `shouldEqual` true
                