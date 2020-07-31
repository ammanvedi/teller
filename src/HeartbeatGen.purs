module HeartbeatGen where

import Data.Array (index, (..))
import Data.Date (Date, Day, Weekday(..), adjust, day, diff, lastDayOfMonth, month, weekday, year)
import Data.Date.Component (Weekday(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import GenTypes (HeartbeatGeneratorFn)
import Prelude (($), (<>), (==), (-), (<), (&&), bottom)
import Trend (HeartbeatMatcher(..), TrendDescription(..))

safeAdjust :: Days -> Date -> Date
safeAdjust d dt =
    case maybeRes of
        Just dtx -> dtx
        Nothing -> dt
    where
        maybeRes = adjust d dt

generateHeartbeat :: Date -> Date -> HeartbeatGeneratorFn -> Array Int
generateHeartbeat dStart dEnd genFunc =
    foldl (\ acc dateIndex -> acc <> [(genFunc (safeAdjust (Days $ toNumber dateIndex) dStart))] ) [] dateIndexes
    where
        dateDelta :: Days
        dateDelta = diff dEnd dStart
        deltaInt = floor $ unwrap dateDelta
        dateIndexes = 0..deltaInt

-- Generators
genEveryDay :: HeartbeatGeneratorFn
genEveryDay dt = 1

genWeekday :: HeartbeatGeneratorFn
genWeekday dt =
    case wkd of
        Monday -> 1
        Tuesday -> 1
        Wednesday -> 1
        Thursday -> 1
        Friday -> 1
        Saturday -> 0
        Sunday -> 0
    where
        wkd = weekday dt

genWeekend :: HeartbeatGeneratorFn
genWeekend dt =
    case wkd of
        Monday -> 0
        Tuesday -> 0
        Wednesday -> 0
        Thursday -> 0
        Friday -> 0
        Saturday -> 1
        Sunday -> 1
    where
        wkd = weekday dt

genSpecificWeekday :: Weekday -> HeartbeatGeneratorFn
genSpecificWeekday w dt =
    if wkd == w
        then 1
        else 0
    where
        wkd = weekday dt

genLastWeekDay :: Weekday -> HeartbeatGeneratorFn
genLastWeekDay w dt = 
    if isSameDay && (daysUntilEndOfMonth < 7)
        then 1
        else 0
    where
        isSameDay = weekday dt == w
        dayOfMonth = fromEnum $ day dt
        lastDateOfMonth = fromEnum $ lastDayOfMonth (year dt) (month dt)
        daysUntilEndOfMonth = lastDateOfMonth - dayOfMonth

genXthDayOfMonth :: Int -> HeartbeatGeneratorFn
genXthDayOfMonth x dt =
    if xAsDay == dateDay then 1 else 0
    where
        xAsDay :: Day
        xAsDay = fromMaybe bottom (toEnum x)
        dateDay = day dt

-- Xth Day of month

createMonthlyMatchers :: Array HeartbeatMatcher
createMonthlyMatchers =
    map (\ i -> 
        HeartbeatMatcher (
            Tuple (genXthDayOfMonth i) 
            (MonthDayTrendDescription { dayOfMonth: i })
        ) ) (1..31)

-- Specific Weekdays

genEveryMonday :: HeartbeatGeneratorFn
genEveryMonday = genSpecificWeekday Monday

genEveryTuesday :: HeartbeatGeneratorFn
genEveryTuesday = genSpecificWeekday Tuesday

genEveryWednesday :: HeartbeatGeneratorFn
genEveryWednesday = genSpecificWeekday Wednesday

genEveryThursday :: HeartbeatGeneratorFn
genEveryThursday = genSpecificWeekday Thursday

genEveryFriday :: HeartbeatGeneratorFn
genEveryFriday = genSpecificWeekday Friday

genEverySaturday :: HeartbeatGeneratorFn
genEverySaturday = genSpecificWeekday Saturday

genEverySunday :: HeartbeatGeneratorFn
genEverySunday = genSpecificWeekday Sunday

-- Last weekday of month

genLastFridayOfMonth :: HeartbeatGeneratorFn
genLastFridayOfMonth = genLastWeekDay Friday

-- Weekdays

-- Weekends

-- TODO - create functions for each trend that return 
-- HeartbeatMatchers assemble into one large array
-- use this to match against trends