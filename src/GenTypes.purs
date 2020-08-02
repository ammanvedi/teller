module GenTypes where

import Data.Date (Date, Weekday)
import Data.Tuple (Tuple)

type HeartbeatGeneratorFn = Date -> Int

data TrendDescription 
    = MonthDayTrendDescription {dayOfMonth :: Int}
    | LastWeekdayTrendDescription {weekday :: Weekday}
    | SpecificWeekdayTrendDescription {weekday :: Weekday}
    | EveryWeekdayTrendDescription
    | WeekendTrendDescription

newtype HeartbeatMatcher = 
    HeartbeatMatcher (Tuple HeartbeatGeneratorFn TrendDescription)