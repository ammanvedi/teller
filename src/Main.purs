module Data.Teller (
    forecast,
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
import Data.Teller.Forecast (forecast) as F

identifyTrends = T.identifyTrends

forecast = F.forecast

isMonthDayTrendDescription = GT.isMonthDayTrendDescription
isLastWeekdayTrendDescription = GT.isLastWeekdayTrendDescription
isSpecificWeekdayTrendDescription = GT.isSpecificWeekdayTrendDescription
isWeekdayTrendDescription = GT.isWeekdayTrendDescription
isEveryWeekdayTrendDescription = GT.isEveryWeekdayTrendDescription
isWeekendTrendDescription = GT.isWeekendTrendDescription