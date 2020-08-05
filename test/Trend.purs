module Test.Trend where

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Teller.GenTypes (HeartbeatMatchResult(..), TrendDescription(..))
import Prelude (Unit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Teller.Transaction (TransactionRec(..))
import Data.Teller.Trend (getMatcherResults)

testTransactions :: Array TransactionRec
testTransactions = [
    TransactionRec ({
            accountId: "acc",
            timestamp: 1596240000000.0, -- 1 aug
            amount: 100.0,
            merchantName: "Barclays",
            reference: "TFL X"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1598918400000.0, -- 1 sep
            amount: 1000.0,
            merchantName: "Barclays",
            reference: "BAR XY"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1601510400000.0, -- 1 oct
            amount: 1.0,
            merchantName: "Barclays",
            reference: "BAR XY"
        })
]

trendSpec :: Spec Unit
trendSpec =
    describe "Trend" do 
        describe "getMatcherResults" do
            it "calculates correct results for a trend that occurs on the first of the month" do
                let res = getMatcherResults testTransactions
                case head res of
                    (Just h) -> 
                        h `shouldEqual` (
                            HeartbeatMatchResult 
                                (Tuple 100.0 
                                    (MonthDayTrendDescription {dayOfMonth: 1} )
                                )
                            )
                    Nothing -> false `shouldEqual` true