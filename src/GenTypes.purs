module Data.Teller.GenTypes where

import Data.Date (Date, Weekday)
import Data.Eq (class Eq)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Prelude (Ordering(..), otherwise, (<>), (==), (>), (<))

type HeartbeatGeneratorFn = Date -> Int

data TrendDescription 
    = MonthDayTrendDescription {dayOfMonth :: Int}
    | LastWeekdayTrendDescription {weekday :: Int}
    | SpecificWeekdayTrendDescription {weekday :: Int}
    | WeekdayTrendDescription {weekdays :: (Array Int)}
    | EveryWeekdayTrendDescription
    | WeekendTrendDescription

instance trendDescriptionShow :: Show TrendDescription where
    show (WeekdayTrendDescription desc) = "Occurs on these weekdays " <> show desc.weekdays
    show (MonthDayTrendDescription desc) = "Occurs every month on " <> show desc.dayOfMonth
    show (LastWeekdayTrendDescription desc) = "Occurs on the last " <> (show desc.weekday) <> "of each month"
    show (SpecificWeekdayTrendDescription desc) = "Occurs every week on " <> (show desc.weekday)
    show EveryWeekdayTrendDescription = "Occurs on weekdays"
    show WeekendTrendDescription = "Occurs on saturdays and sundays"

instance trendDescriptionEq :: Eq TrendDescription where
    eq t1 t2 = (show t1) == (show t2)

newtype HeartbeatMatcher = 
    HeartbeatMatcher (Tuple HeartbeatGeneratorFn TrendDescription)

newtype HeartbeatMatchResult = HeartbeatMatchResult (Tuple Number TrendDescription)

derive instance heartbeatMatchResultNewType :: Newtype HeartbeatMatchResult _

instance heartbeatMatchResultShow :: Show HeartbeatMatchResult where
    show (HeartbeatMatchResult (Tuple confidence description))
        = "{ " <> show description <> " | confidence " <> (show confidence) <> " }"

instance heartbeatMatchResultEq :: Eq HeartbeatMatchResult where
    eq t1 t2 = (show t1) == (show t2)

instance heartbeatMatchResultOrd :: Ord HeartbeatMatchResult where
    compare
        (HeartbeatMatchResult (Tuple num1 desc1))
        (HeartbeatMatchResult (Tuple num2 desc2))
            | num1 > num2 = GT
            | num1 < num2 = LT
            | otherwise = EQ