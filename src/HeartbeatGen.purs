module Data.Teller.HeartbeatGen where

import Data.Array ((..), (!!), elemIndex)
import Data.Date (Date, Day, adjust, day, diff, lastDayOfMonth, month, weekday, year)
import Data.Date.Component (Weekday(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Functor (map)
import Data.Int (binary, floor, toNumber, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (length)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Teller.DateHelpers (iterateDateRange)
import Data.Teller.GenTypes (HeartbeatGeneratorFn, HeartbeatMatcher(..), TrendDescription(..))
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Math (pow)
import Prelude (bottom, ($), (&&), (+), (-), (<), (<>), (==), (>=))

padStringLeftWith :: String -> String -> Int -> String
padStringLeftWith s _ minLeng
    | length s >= minLeng = s
padStringLeftWith s padder minLeng = padStringLeftWith (padder <> s) padder minLeng

numberToPaddedBinaryString :: Int -> Int -> String
numberToPaddedBinaryString num length =
    padStringLeftWith asBinary "0" length
    where
        asBinary = toStringAs binary num

binParseInt :: String -> Int
binParseInt s =
    case s of
        "0" -> 0
        "1" -> 1
        otherwise -> 0

binaryStringToInts :: String -> Array Int
binaryStringToInts s = 
    map binParseInt splitString
    where
        splitString = split (Pattern "") s

createAllBinaryPermutationsOfLength :: Int -> Array (Array Int)
createAllBinaryPermutationsOfLength i =
    map binaryStringToInts binaryStrings
    where
        binMax = floor $ (pow 2.0 (toNumber i)) - 1.0
        numList = (1..(binMax))
        binaryStrings = map (\xi -> numberToPaddedBinaryString xi i) numList

generateHeartbeat :: Date -> Date -> HeartbeatGeneratorFn -> Array Int
generateHeartbeat dStart dEnd genFunc =
    iterateDateRange dStart dEnd genFunc

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
            (MonthDayTrendDescription { dayOfMonth: i, pricing: 0.0 })
        ) ) (1..31)

weekdayNumbersToBinarySequence :: Array Int -> Array Int
weekdayNumbersToBinarySequence xs = 
    map (\i -> (
        case elemIndex i xs of
            (Just _) -> 1
            Nothing -> 0
    )) (1..7)

binaryWeekPatternToMatcher :: Array Int -> HeartbeatGeneratorFn
binaryWeekPatternToMatcher xs dt = 
    case shouldOccur of
        (Just x) -> x
        Nothing -> 0
    where
        wkd = weekday dt
        wkdIndex = (fromEnum wkd) - 1
        shouldOccur = xs !! wkdIndex

getOccurringWeekdays :: Array Int -> Array Weekday
getOccurringWeekdays xs = 
    foldlWithIndex (\ix acc xi -> 
        case xi of
            1 -> case toEnum (ix + 1) of
                (Just w) -> acc <> [w]
                Nothing -> acc
            otherwise -> acc
    ) [] xs

-- Create matchers for every possible weekday combination
-- for a single week

createWeekdayMatchers :: Array HeartbeatMatcher
createWeekdayMatchers =
    map (\binSeq -> HeartbeatMatcher (
        Tuple 
            (binaryWeekPatternToMatcher binSeq)
            (WeekdayTrendDescription {weekdays: (map fromEnum  $ getOccurringWeekdays binSeq), pricing: [] })
    )) binarySequences
    where
        binarySequences = createAllBinaryPermutationsOfLength 7


-- Specific Weekdays

genEveryMondayMatcher :: HeartbeatMatcher
genEveryMondayMatcher = HeartbeatMatcher (
            Tuple (genSpecificWeekday Monday) 
(SpecificWeekdayTrendDescription { weekday: fromEnum Monday, pricing: 0.0 }))

genEveryTuesdayMatcher :: HeartbeatMatcher
genEveryTuesdayMatcher = HeartbeatMatcher (
            Tuple (genSpecificWeekday Tuesday) 
(SpecificWeekdayTrendDescription { weekday: fromEnum Tuesday, pricing: 0.0 }))

genEveryWednesdayMatcher :: HeartbeatMatcher
genEveryWednesdayMatcher = HeartbeatMatcher (
            Tuple (genSpecificWeekday Wednesday) 
(SpecificWeekdayTrendDescription { weekday: fromEnum Wednesday, pricing: 0.0 }))

genEveryThursdayMatcher :: HeartbeatMatcher
genEveryThursdayMatcher = HeartbeatMatcher (
            Tuple (genSpecificWeekday Thursday) 
(SpecificWeekdayTrendDescription { weekday: fromEnum Thursday, pricing: 0.0 }))

genEveryFridayMatcher :: HeartbeatMatcher
genEveryFridayMatcher = HeartbeatMatcher (
            Tuple (genSpecificWeekday Friday) 
(SpecificWeekdayTrendDescription { weekday: fromEnum Friday, pricing: 0.0 }))

genEverySaturdayMatcher :: HeartbeatMatcher
genEverySaturdayMatcher = HeartbeatMatcher (
            Tuple (genSpecificWeekday Saturday) 
(SpecificWeekdayTrendDescription { weekday: fromEnum Saturday, pricing: 0.0 }))

genEverySundayMatcher :: HeartbeatMatcher
genEverySundayMatcher = HeartbeatMatcher (
            Tuple (genSpecificWeekday Sunday) 
(SpecificWeekdayTrendDescription { weekday: fromEnum Sunday, pricing: 0.0 }))

-- Last weekday of month

genLastFridayOfMonthMatcher :: HeartbeatMatcher
genLastFridayOfMonthMatcher = HeartbeatMatcher (
            Tuple (genLastWeekDay Friday) 
(LastWeekdayTrendDescription { weekday: fromEnum Friday, pricing: 0.0 }))

-- Weekdays

genWeekdaysMatcher :: HeartbeatMatcher
genWeekdaysMatcher = HeartbeatMatcher (Tuple genWeekday (EveryWeekdayTrendDescription {pricing: []}))

-- Weekends

genWeekendMatcher :: HeartbeatMatcher
genWeekendMatcher = HeartbeatMatcher (Tuple genWeekend (WeekendTrendDescription {pricing: []}))

allHeartbeatMatchers :: Array HeartbeatMatcher
allHeartbeatMatchers = 
    createMonthlyMatchers <> createWeekdayMatchers <> [
        genLastFridayOfMonthMatcher,
        genWeekdaysMatcher,
        genWeekendMatcher
    ]