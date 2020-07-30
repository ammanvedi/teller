module HeartbeatGen where

import Data.Array ((..))
import Data.Date (Date, Weekday(..), adjust, diff, weekday)
import Data.Date.Component (Weekday(..))
import Data.Date.Component.Gen (genWeekday)
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..))
import Prelude (($), (<>))

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
        dateDelta = diff dStart dEnd
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
