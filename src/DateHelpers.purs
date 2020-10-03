module Data.Teller.DateHelpers where

import Prelude

import Data.Array ((..))
import Data.Date (Date, adjust, canonicalDate, diff, month, year)
import Data.Enum (toEnum)
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..))

getStartOfMonth :: Date -> Maybe Date
getStartOfMonth nowDate = do
    year <- Just $ year nowDate
    month <- Just $ month nowDate
    day <- toEnum 1
    pure $ canonicalDate year month day

getXDaysPrior :: (Maybe Date) -> Int -> Maybe Date
getXDaysPrior nowDate priorDays = 
    case nowDate of 
        (Just d) -> do
            days <- Just $ Days $ toNumber priorDays
            adjusted <- adjust days d
            pure adjusted
        Nothing -> Nothing

maybeDateToString :: Maybe Date -> String
maybeDateToString d = 
    case d of 
        (Just dt) -> show dt
        Nothing -> "NODATE"

safeAdjust :: Days -> Date -> Date
safeAdjust d dt =
    case maybeRes of
        Just dtx -> dtx
        Nothing -> dt
    where
        maybeRes = adjust d dt

iterateDateRange :: forall a. Date -> Date -> (Date -> a) -> Array a
iterateDateRange dStart dEnd mapper =
    foldl (\ acc dateIndex -> acc <> [(mapper (safeAdjust (Days $ toNumber dateIndex) dStart))] ) [] dateIndexes
    where
        dateDelta :: Days
        dateDelta = diff dEnd dStart
        deltaInt = floor $ unwrap dateDelta
        dateIndexes = 0..deltaInt