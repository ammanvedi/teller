module Test.Trend where

import Data.Array (head, length)
import Data.Maybe (Maybe(..))
import Data.Teller.GenTypes (HeartbeatMatchResult(..), TrendDescription(..))
import Data.Teller.Transaction (TransactionRec(..))
import Data.Teller.Trend (getMatcherResults, identifyTrends)
import Data.Tuple (Tuple(..))
import Prelude (Unit, discard)
import Test.Fixtures (realTransactions)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Show (show)

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
                                    (MonthDayTrendDescription {dayOfMonth: 1, pricing: 0.0} )
                                )
                            )
                    Nothing -> false `shouldEqual` true
        describe "identifyTrends" do
            it "identifies trends from a large dataset of real transactions" do
                let res = identifyTrends realTransactions
                (show res) `shouldEqual` "[(Tuple \"201053 53364402 MOBILE-CHANNEL FT\" Occurs every month on 20 at price -60.0),(Tuple \"201815 30238074 MOBILE-CHANNEL FT\" Occurs every month on 7 at price -40.0),(Tuple \"AMMAN VEDI STO AMMAN VEDI STO\" Occurs every month on 5 at price 1000.0),(Tuple \"APPLE.COM/BILL IRELAND\" Occurs every month on 20 at price 3.49),(Tuple \"Amazon\" Occurs every month on 17 at price 3.49),(Tuple \"BT Group\" Occurs every month on 28 at price 24.99),(Tuple \"CO-OP\" Occurs every month on 8 at price 1.15),(Tuple \"Direct Line\" Occurs every month on 31 at price 18.48),(Tuple \"E.ON 016405488580A DDR E.ON\" Occurs every month on 31 at price 31.0),(Tuple \"Google Play\" Occurs every month on 28 at price 2.49),(Tuple \"JetBrains CZECH REP\" Occurs every month on 23 at price 4.32),(Tuple \"LB EALING 57428263 DDR London Borough of Ealing\" Occurs every month on 31 at price 90.48),(Tuple \"Netflix\" Occurs every month on 21 at price 11.99),(Tuple \"Nike\" Occurs every month on 27 at price -88.95),(Tuple \"Ocado\" Occurs every month on 16 at price 6.99),(Tuple \"QUALITY FOODS\" Occurs every month on 19 at price 7.1),(Tuple \"SANTANDER MORTGAGE 043697428 DDR Santander\" Occurs every month on 31 at price 694.52),(Tuple \"TESCO STORES 6836\" Occurs every month on 7 at price 6.4),(Tuple \"TV LICENSING\" Occurs every month on 2 at price 30.9),(Tuple \"Tidal Music SWEDEN\" Occurs every month on 20 at price 9.99),(Tuple \"Transport for London\" Occurs on these weekdays [2,5,6,7] at price [6.7,7.8,3.2,2.8]),(Tuple \"Vodafone\" Occurs every month on 30 at price 51.0),(Tuple \"WORK A T LTD Lifeworks BGC\" Occurs on the last 5 of each month  at price -2919.58)]"
