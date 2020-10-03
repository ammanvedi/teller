module Data.Teller.Forecast where

import Data.Array (length, snoc, union)
import Data.Date (Date, Weekday(..))
import Data.DateTime.Instant (fromDate, unInstant)
import Data.Enum (toEnumWithDefaults)
import Data.Eq (class Eq, (==))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Teller.DateHelpers (dateFromMs, iterateDateRange)
import Data.Teller.GenTypes (HeartbeatGeneratorFn, TrendDescription(..))
import Data.Teller.HeartbeatGen (binaryWeekPatternToMatcher, genLastWeekDay, genSpecificWeekday, genWeekday, genWeekend, genXthDayOfMonth, weekdayNumbersToBinarySequence)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Prelude (show, ($), (<>), (&&), bind, pure)

newtype TrendDescriptionStruct = TrendDescriptionStruct {
    id :: String,
    trend :: TrendDescription,
    merchant :: String
}

instance trendDescriptionStructShow :: Show TrendDescriptionStruct where
    show (TrendDescriptionStruct {id, trend, merchant}) = 
        id <> "_" <> (show trend) <> "_" <> merchant

instance trendDescriptionStructEq :: Eq TrendDescriptionStruct where
    eq (TrendDescriptionStruct {id: idA}) (TrendDescriptionStruct {id: idB}) = idA == idB

newtype ForecastedDay = ForecastedDay {
    dateTimestampMs :: Number,
    trendIds :: Array String
}

haveSameContents :: forall a. Eq a => Array a -> Array a -> Boolean
haveSameContents xs ys = 
    (lenU == lenX) && (lenU == lenY)
    where 
        lenX = length xs
        lenY = length ys
        u = union xs ys
        lenU = length u

instance eqForecastedDay :: Eq ForecastedDay where
    eq 
        (ForecastedDay {dateTimestampMs: tA, trendIds: idA})
        (ForecastedDay {dateTimestampMs: tB, trendIds: idB}) = 
            (tA == tB) && (haveSameContents idA idB)

instance forecastedDayShow :: Show ForecastedDay where
    show (ForecastedDay {dateTimestampMs, trendIds}) =
        (show dateTimestampMs) <> (show trendIds)

newtype Forecast = Forecast {
    days :: Array ForecastedDay
}

instance eqForecast :: Eq Forecast where
    eq 
        (Forecast {days: dA})
        (Forecast {days: dB}) =
            haveSameContents dA dB

instance forecastShow :: Show Forecast where
    show (Forecast {days}) = 
        show days

trendTupleToStruct :: Tuple String TrendDescription -> TrendDescriptionStruct
trendTupleToStruct (Tuple merchant trend) = 
    TrendDescriptionStruct {
        id: generateTrendId (Tuple merchant trend),
        trend: trend,
        merchant: merchant
    }

createMatcher :: TrendDescription -> HeartbeatGeneratorFn
createMatcher trend = 
    case trend of
        (MonthDayTrendDescription { dayOfMonth }) -> genXthDayOfMonth dayOfMonth
        (LastWeekdayTrendDescription { weekday }) -> genLastWeekDay $ toEnumWithDefaults Monday Friday weekday
        (SpecificWeekdayTrendDescription { weekday }) -> genSpecificWeekday $ toEnumWithDefaults Monday Friday weekday
        (WeekdayTrendDescription { weekdays }) -> binaryWeekPatternToMatcher $ weekdayNumbersToBinarySequence weekdays
        (EveryWeekdayTrendDescription {}) -> genWeekday
        (WeekendTrendDescription {}) -> genWeekend


generateTrendId :: Tuple String TrendDescription -> String
generateTrendId (Tuple merchant trend) = 
    case trend of
        (MonthDayTrendDescription { dayOfMonth }) -> "MonthDay_" <> (show dayOfMonth) <> "_" <> merchant
        (LastWeekdayTrendDescription { weekday }) -> "LastWeekday_" <> (show weekday) <> "_" <> merchant
        (SpecificWeekdayTrendDescription { weekday }) -> "SpecificWeekday_" <> (show weekday) <> "_" <> merchant
        (WeekdayTrendDescription { weekdays }) -> "Weekday_" <> (show weekdays) <> "_" <> merchant
        (EveryWeekdayTrendDescription {}) -> "EveryWeekday_" <> merchant
        (WeekendTrendDescription {}) -> "Weekend_" <> merchant

doesTrendOccurOnDay :: TrendDescription -> Date -> Boolean
doesTrendOccurOnDay trend dt = 
    case matcher dt of
        1 -> true
        0 -> false
        otherwise -> false
    where
        matcher = createMatcher trend

pickTrendsOccurringOnDay :: Date -> Array TrendDescriptionStruct -> Array String
pickTrendsOccurringOnDay dt ts = 
    foldl (\ids (TrendDescriptionStruct {trend, id}) -> (
        case doesTrendOccurOnDay trend dt of
            true -> snoc ids id
            false -> ids
    ) ) [] ts

createForecastForDay :: Array TrendDescriptionStruct -> Date -> ForecastedDay
createForecastForDay ts dt = 
    ForecastedDay {
        dateTimestampMs: ms,
        trendIds: ids
    }
    where
        instant = fromDate dt
        (Milliseconds ms) = unInstant instant
        ids = pickTrendsOccurringOnDay dt ts

-- Holy grail function
forecast :: Number -> Number -> Array TrendDescriptionStruct -> Forecast
forecast sDate eDate trends = do
    case forecastedDays of 
        (Just f) -> Forecast { days: f }
        Nothing -> Forecast { days: [] }

    where 
        dayForecaster = createForecastForDay trends
        forecastedDays = do
            s <- dateFromMs sDate
            e <- dateFromMs eDate
            pure $ iterateDateRange s e dayForecaster