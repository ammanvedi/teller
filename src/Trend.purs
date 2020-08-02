module Trend where

import Data.Array (head, last)
import Data.Date (Date)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GenTypes (HeartbeatMatcher(..), TrendDescription)
import HeartbeatGen (allHeartbeatMatchers, generateHeartbeat)
import Prelude (bind, pure)
import SignalProcessing (naiveSignalMatch)
import Transaction (TransactionRec, getBinaryHeartbeat, transactionDate)

-- Holy grail function
-- identifyTrend :: Array TransactionRec -> Maybe TrendDescription

matchAgainstHeartbeat :: Date -> Date -> Array Int -> HeartbeatMatcher -> Tuple Number TrendDescription
matchAgainstHeartbeat dStart dEnd hb (HeartbeatMatcher (Tuple genFunc desc)) =
    Tuple signalMatch desc
    where
        generatedHb = generateHeartbeat dStart dEnd genFunc
        signalMatch = naiveSignalMatch generatedHb hb

getMatcherResults :: Array TransactionRec -> Array (Tuple Number TrendDescription)
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


