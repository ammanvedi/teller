module DateHelpers where

import Prelude
import Data.Date (Date, adjust, canonicalDate, month, year)
import Data.Enum (toEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
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