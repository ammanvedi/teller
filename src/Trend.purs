module Data.Teller.Trend where

import Debug.Trace

import Data.Array (fromFoldable, head, last, reverse, sort)
import Data.Date (Date)
import Data.Foldable (foldl, maximum)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Teller.GenTypes (HeartbeatMatchResult(..), HeartbeatMatcher(..), TrendDescription)
import Data.Teller.HeartbeatGen (allHeartbeatMatchers, generateHeartbeat)
import Data.Teller.SignalProcessing (estimatePeriod, gilbertAndWellsSimilarity, naiveSignalMatch)
import Data.Teller.Transaction (TransactionRec, getBinaryHeartbeat, transactionDate, transactionsForMerchant, uniqueMerchants)
import Data.Tuple (Tuple(..), snd)
import Prelude (bind, pure, ($), (<), (<>))

identifyTrend :: String -> Array TransactionRec -> Maybe TrendDescription
identifyTrend m xs =
    if period < 32.0
        then do
            matchers <- pure $ spy "mr" (reverse $ sort $ getMatcherResults xs)
            maxMatch <- maximum matchers
            pure $ snd $ unwrap maxMatch
        else
            Nothing
    where
        heartbeat = getBinaryHeartbeat xs
        period = estimatePeriod heartbeat

-- Holy grail function
identifyTrends :: Array TransactionRec -> Array (Tuple String TrendDescription)
identifyTrends xs =
    foldl (\ acc merchant -> 
        case (identifyTrend merchant $ sort $ transactionsForMerchant xs merchant) of
            (Just t) -> acc <> [Tuple merchant t]
            Nothing -> acc
        ) [] merchants
    where
        merchants = fromFoldable $ uniqueMerchants xs

matchAgainstHeartbeat :: Date -> Date -> Array Int -> HeartbeatMatcher -> HeartbeatMatchResult
matchAgainstHeartbeat dStart dEnd hb (HeartbeatMatcher (Tuple genFunc desc)) =
    HeartbeatMatchResult $ Tuple signalMatch desc
    where
        generatedHb = generateHeartbeat dStart dEnd genFunc
        signalMatch = naiveSignalMatch generatedHb hb

getMatcherResults :: Array TransactionRec -> Array HeartbeatMatchResult
getMatcherResults [] = []
getMatcherResults [_] = []
getMatcherResults xs =
    case dates of
        (Just (Tuple startDate endDate)) ->
            map (\ matcher -> matchAgainstHeartbeat startDate endDate hb matcher) allHeartbeatMatchers
        Nothing -> []
    where
        hb = getBinaryHeartbeat xs
        dates :: Maybe (Tuple Date Date)
        dates = do
            startTrans <- head xs
            endTrans <- last xs
            startDate <- transactionDate startTrans
            endDate <- transactionDate endTrans
            pure (Tuple startDate endDate)


-- TODO: More test cases for getMatcherResults
-- calculate the price that is being paid for a trend
-- create pipeline, split by merchant -> identify if trend is random process through autocorrelation-> find best match trend for each 
-- given a transaction does it belong to a trend from a given set of trends

-- need to either have matchers for every combination of days in a week
-- or need to allow the top N matches through
-- this is exemplified by the train matcher only atm returning that the transaction occurrs every tuesday
