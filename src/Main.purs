module Data.Teller (
    identifyTrends,
    isMonthDayTrendDescription,
    isLastWeekdayTrendDescription,
    isSpecificWeekdayTrendDescription,
    isWeekdayTrendDescription,
    isEveryWeekdayTrendDescription,
    isWeekendTrendDescription
) where 

import Data.Teller.GenTypes as GT
import Data.Teller.Trend (identifyTrends) as T

identifyTrends = T.identifyTrends

isMonthDayTrendDescription = GT.isMonthDayTrendDescription
isLastWeekdayTrendDescription = GT.isLastWeekdayTrendDescription
isSpecificWeekdayTrendDescription = GT.isSpecificWeekdayTrendDescription
isWeekdayTrendDescription = GT.isWeekdayTrendDescription
isEveryWeekdayTrendDescription = GT.isEveryWeekdayTrendDescription
isWeekendTrendDescription = GT.isWeekendTrendDescription

x = "test"