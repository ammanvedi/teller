module DateHelpers where

import Data.Date
import Data.Time.Duration
import Data.Enum
import Data.Int
import Data.Maybe
import Data.Number.Format
import Data.Ord
import Data.Ordering
import Prelude

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
        (Just d) -> show d
        Nothing -> "NODATE"