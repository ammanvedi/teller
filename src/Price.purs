module Data.Teller.Price where

import Data.Array (last)
import Data.Date.Component (Weekday(..))
import Data.Enum (toEnumWithDefaults)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Teller.GenTypes (TrendDescription(..))
import Data.Teller.Transaction (TransactionRec(..), getTransactionsForWeekDay)
import Prelude (($), (<>))

addPricingToTrend :: Array TransactionRec -> TrendDescription -> TrendDescription
addPricingToTrend tx (MonthDayTrendDescription desc) 
    = MonthDayTrendDescription $ desc {pricing = getLastAmount tx}
addPricingToTrend tx (LastWeekdayTrendDescription desc) 
    = LastWeekdayTrendDescription $ desc {pricing = getLastAmount tx}
addPricingToTrend tx (SpecificWeekdayTrendDescription desc) 
    = SpecificWeekdayTrendDescription $ desc {pricing = getLastAmount tx}
addPricingToTrend tx (WeekdayTrendDescription desc) 
    = WeekdayTrendDescription $ desc {pricing =  
        getPricingForWeekdays (intArrayToWeekdayArray desc.weekdays) tx
    }
addPricingToTrend tx (EveryWeekdayTrendDescription desc) 
    = EveryWeekdayTrendDescription $ desc {pricing = getPricingForWeekdays [Monday, Tuesday, Wednesday, Thursday, Friday] tx}
addPricingToTrend tx (WeekendTrendDescription desc) 
    = WeekendTrendDescription $ desc {pricing = getPricingForWeekdays [Saturday, Sunday] tx}

type SingleValuePricingStrategy = Array TransactionRec -> Number

getLastAmount :: SingleValuePricingStrategy
getLastAmount xs = 
    case last xs of
        (Just (TransactionRec t)) -> t.amount
        Nothing -> 0.0

getPricingForWeekdays :: Array Weekday -> Array TransactionRec -> Array Number
getPricingForWeekdays xs tx = 
    map (\w -> getLastAmount $ getTransactionsForWeekDay tx w) xs

intArrayToWeekdayArray :: Array Int -> Array Weekday
intArrayToWeekdayArray ints =
    foldl (\acc i -> acc <> [toEnumWithDefaults Monday Sunday i]) [] ints