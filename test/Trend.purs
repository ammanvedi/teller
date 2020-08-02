module Test.Trend where

import Prelude (Unit, discard, negate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Transaction (testTransaction)
import Transaction (TransactionRec(..))
import Trend (getMatcherResults)

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
            it "calculates correct results" do
                let res = getMatcherResults testTransactions
                res `shouldEqual` []
