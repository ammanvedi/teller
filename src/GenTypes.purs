module Data.Teller.GenTypes where

import Data.Date (Date)
import Data.Eq (class Eq)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Prelude (Ordering(..), otherwise, (<>), (==), (>), (<))

type HeartbeatGeneratorFn = Date -> Int

data TrendDescription 
    = MonthDayTrendDescription {
            dayOfMonth :: Int,
            pricing :: Number
        }
    | LastWeekdayTrendDescription {
            weekday :: Int,
            pricing :: Number
        }
    | SpecificWeekdayTrendDescription {
            weekday :: Int,
            pricing :: Number
        }
    | WeekdayTrendDescription {
            weekdays :: (Array Int),
            pricing :: (Array Number)
        }
    | EveryWeekdayTrendDescription {
            pricing :: (Array Number)
        }
    | WeekendTrendDescription {
            pricing :: (Array Number)
        }

-- Since we have trouble exporting the type constructor
-- we define functions we can use to determine if a
-- trenddescription instance is a specific sub constructor

isMonthDayTrendDescription :: TrendDescription -> Boolean
isMonthDayTrendDescription t =
    case t of
        (MonthDayTrendDescription _) -> true
        otherwise -> false

isLastWeekdayTrendDescription :: TrendDescription -> Boolean
isLastWeekdayTrendDescription t =
    case t of
        (LastWeekdayTrendDescription _) -> true
        otherwise -> false

isSpecificWeekdayTrendDescription :: TrendDescription -> Boolean
isSpecificWeekdayTrendDescription t =
    case t of
        (SpecificWeekdayTrendDescription _) -> true
        otherwise -> false

isWeekdayTrendDescription :: TrendDescription -> Boolean
isWeekdayTrendDescription t =
    case t of
        (WeekdayTrendDescription _) -> true
        otherwise -> false

isEveryWeekdayTrendDescription :: TrendDescription -> Boolean
isEveryWeekdayTrendDescription t =
    case t of
        (EveryWeekdayTrendDescription _) -> true
        otherwise -> false

isWeekendTrendDescription :: TrendDescription -> Boolean
isWeekendTrendDescription t =
    case t of
        (WeekendTrendDescription _) -> true
        otherwise -> false

instance trendDescriptionShow :: Show TrendDescription where
    show (WeekdayTrendDescription desc) = "Occurs on these weekdays " <> show desc.weekdays <> " at price " <> show desc.pricing
    show (MonthDayTrendDescription desc) = "Occurs every month on " <> show desc.dayOfMonth <> " at price " <> show desc.pricing
    show (LastWeekdayTrendDescription desc) = "Occurs on the last " <> (show desc.weekday) <> " of each month " <> " at price " <> show desc.pricing
    show (SpecificWeekdayTrendDescription desc) = "Occurs every week on " <> (show desc.weekday) <> " at price " <> show desc.pricing
    show (EveryWeekdayTrendDescription desc) = "Occurs on weekdays" <> " at price " <> show desc.pricing
    show (WeekendTrendDescription desc) = "Occurs on saturdays and sundays" <> " at price " <> show desc.pricing

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