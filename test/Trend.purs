module Test.Trend where

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Teller.GenTypes (HeartbeatMatchResult(..), TrendDescription(..))
import Data.Teller.Transaction (TransactionRec(..))
import Data.Teller.Trend (getMatcherResults, identifyTrends)
import Data.Tuple (Tuple(..))
import Prelude (Unit, discard)
import Test.Fixtures (realTransactions)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
                                (Tuple 1.0 
                                    (MonthDayTrendDescription {dayOfMonth: 1, pricing: 0.0} )
                                )
                            )
                    Nothing -> false `shouldEqual` true
        describe "identifyTrends" do
            it "identifies trends from a large dataset of real transactions" do
                let res = identifyTrends realTransactions
                (show res) `shouldEqual` "[MonthDay_20_201053 53364402 MOBILE-CHANNEL FT_Occurs every month on 20 at price -60.0_201053 53364402 MOBILE-CHANNEL FT,MonthDay_20_201815 30238074 MOBILE-CHANNEL FT_Occurs every month on 20 at price -40.0_201815 30238074 MOBILE-CHANNEL FT,MonthDay_5_AMMAN VEDI STO AMMAN VEDI STO_Occurs every month on 5 at price 1000.0_AMMAN VEDI STO AMMAN VEDI STO,MonthDay_20_APPLE.COM/BILL IRELAND_Occurs every month on 20 at price 3.49_APPLE.COM/BILL IRELAND,MonthDay_17_Amazon_Occurs every month on 17 at price 3.49_Amazon,MonthDay_28_BT Group_Occurs every month on 28 at price 24.99_BT Group,MonthDay_8_CO-OP_Occurs every month on 8 at price 1.15_CO-OP,MonthDay_31_Direct Line_Occurs every month on 31 at price 18.48_Direct Line,MonthDay_31_E.ON 016405488580A DDR E.ON_Occurs every month on 31 at price 31.0_E.ON 016405488580A DDR E.ON,MonthDay_17_Google Play_Occurs every month on 17 at price 2.49_Google Play,MonthDay_23_JetBrains CZECH REP_Occurs every month on 23 at price 4.32_JetBrains CZECH REP,MonthDay_31_LB EALING 57428263 DDR London Borough of Ealing_Occurs every month on 31 at price 90.48_LB EALING 57428263 DDR London Borough of Ealing,MonthDay_21_Netflix_Occurs every month on 21 at price 11.99_Netflix,MonthDay_13_Nike_Occurs every month on 13 at price -88.95_Nike,MonthDay_1_Ocado_Occurs every month on 1 at price 6.99_Ocado,MonthDay_19_QUALITY FOODS_Occurs every month on 19 at price 7.1_QUALITY FOODS,MonthDay_31_SANTANDER MORTGAGE 043697428 DDR Santander_Occurs every month on 31 at price 694.52_SANTANDER MORTGAGE 043697428 DDR Santander,MonthDay_23_TESCO STORES 6836_Occurs every month on 23 at price 6.4_TESCO STORES 6836,MonthDay_2_TV LICENSING_Occurs every month on 2 at price 30.9_TV LICENSING,LastWeekday_5_Test Income_Occurs on the last 5 of each month  at price -2919.58_Test Income,MonthDay_20_Tidal Music SWEDEN_Occurs every month on 20 at price 9.99_Tidal Music SWEDEN,Weekday_[3,7]_Transport for London_Occurs on these weekdays [3,7] at price [7.8,3.0]_Transport for London,MonthDay_30_Vodafone_Occurs every month on 30 at price 51.0_Vodafone]"
