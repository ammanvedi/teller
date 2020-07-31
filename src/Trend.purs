module Trend where

import Data.Date (Weekday)
import Data.Tuple (Tuple)
import GenTypes (HeartbeatGeneratorFn)
import Transaction (TransactionRec(..))

data TrendDescription 
    = MonthDayTrendDescription {dayOfMonth :: Int}
    | LastWeekdayTrendDescription {weekday :: Weekday}
    | SpecificWeekdayTrendDescription {weekday :: Weekday}
    | EveryWeekdayTrendDescription
    | EveryDayTrendDescription
    | WeekendTrendDescription

newtype HeartbeatMatcher = 
    HeartbeatMatcher (Tuple HeartbeatGeneratorFn TrendDescription)



-- identifyTrend :: Array TransactionRec -> 

