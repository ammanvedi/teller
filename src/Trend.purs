module Trend where

import Data.Array (head, last)
import Data.Date (Date)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GenTypes (HeartbeatMatcher(..), HeartbeatMatchResult(..))
import HeartbeatGen (allHeartbeatMatchers, generateHeartbeat)
import Prelude (bind, pure, ($))
import SignalProcessing (naiveSignalMatch)
import Transaction (TransactionRec, getBinaryHeartbeat, transactionDate)

-- Holy grail function
-- identifyTrend :: Array TransactionRec -> Maybe TrendDescription



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