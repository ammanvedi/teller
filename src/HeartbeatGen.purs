module HeartbeatGen where

import Data.Array ((..))
import Data.Date (Date, Day, adjust, day, diff, lastDayOfMonth, month, weekday, year)
import Data.Date.Component (Weekday(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..))
import Prelude (($), (<>), (==), (-), (<), (&&), bottom)

type HeartbeatGeneratorFn = Date -> Int

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

genLastFridayOfMonth :: HeartbeatGeneratorFn
genLastFridayOfMonth = genLastWeekDay Friday

genXthDayOfMonth :: Int -> HeartbeatGeneratorFn
genXthDayOfMonth x dt =
    if xAsDay == dateDay then 1 else 0
    where
        xAsDay :: Day
        xAsDay = fromMaybe bottom (toEnum x)
        dateDay = day dt

firstOfMonth :: HeartbeatGeneratorFn
firstOfMonth = genXthDayOfMonth 1