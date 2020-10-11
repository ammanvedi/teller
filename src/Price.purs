module Data.Teller.Price where

import Data.Date (Date)
import Data.Array (findIndex, last, (!!))
import Data.Date.Component (Weekday(..))
import Data.Enum (toEnumWithDefaults)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Teller.DateHelpers (getWeekdayNumber)
import Data.Teller.GenTypes (TrendDescription(..))
import Data.Teller.Transaction (TransactionRec(..), getTransactionsForWeekDay)
import Prelude (bind, ($), (<>), (==))

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

getPriceForGivenWeekday :: Date -> Array Int -> Array Number -> Number
getPriceForGivenWeekday dt weekdays prices =
    case price of
        (Just p) -> p
        Nothing -> 0.0
    where
        weekdayNum = getWeekdayNumber dt
        price = do
            ix <- findIndex (\i -> i == weekdayNum) weekdays
            prices !! ix

getPricing :: TrendDescription -> Date -> Number
getPricing t dt =
    case t of
        (MonthDayTrendDescription { pricing }) -> pricing
        (LastWeekdayTrendDescription { pricing }) -> pricing
        (SpecificWeekdayTrendDescription { pricing }) -> pricing
        (WeekdayTrendDescription { pricing, weekdays }) ->
            getPriceForGivenWeekday dt weekdays pricing
        (EveryWeekdayTrendDescription { pricing }) ->
            getPriceForGivenWeekday dt [1, 2, 3, 4, 5] pricing
        (WeekendTrendDescription { pricing }) ->
            getPriceForGivenWeekday dt [6, 7] pricing