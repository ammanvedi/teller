module Data.Teller.Trend where

import Data.Array (fromFoldable, head, last, reverse, sort)
import Data.Date (Date)
import Data.Foldable (foldl, maximum)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Teller.GenTypes (HeartbeatMatchResult(..), HeartbeatMatcher(..), TrendDescription)
import Data.Teller.HeartbeatGen (allHeartbeatMatchers, generateHeartbeat)
import Data.Teller.Price (addPricingToTrend)
import Data.Teller.SignalProcessing (estimatePeriod, hammingDistanceWeighted, invertValue)
import Data.Teller.Transaction (TransactionRec, getBinaryHeartbeat, transactionDate, transactionsForMerchant, uniqueMerchants)
import Data.Tuple (Tuple(..), snd)
import Prelude (bind, pure, ($), (<), (<>))

identifyTrend :: String -> Array TransactionRec -> Maybe TrendDescription
identifyTrend m xs =
    if period < 32.0
        then do
            matchers <- pure $ (reverse $ sort $ getMatcherResults xs)
            -- at this point we are going to get matcher results that have the same percentage
            -- and we need some way to deterministically choose between them
            -- perhaps we could bias the match toward values that match later in the string
            -- so that the matcher that was correct most recently is chosen
            -- otherwise we might end up getting different answers depending on
            -- what machine we run code on
            maxMatch <- maximum matchers
            winningTrend <- Just (snd $ unwrap maxMatch)
            pure $ addPricingToTrend xs winningTrend
        else
            Nothing
    where
        heartbeat = getBinaryHeartbeat xs
        period = estimatePeriod heartbeat

-- Holy grail function
identifyTrends :: Array TransactionRec -> Array (Tuple String TrendDescription)
identifyTrends xs =
    foldl (\ acc merchant ->
        case (identifyTrend merchant 
            $ transactionsForMerchant sortedTransactions merchant
            ) of
            (Just t) -> acc <> [Tuple merchant t]
            Nothing -> acc
        ) [] merchants
    where
        sortedTransactions = sort xs
        merchants = fromFoldable $ uniqueMerchants sortedTransactions

matchAgainstHeartbeat :: Date -> Date -> Array Int -> HeartbeatMatcher -> HeartbeatMatchResult
matchAgainstHeartbeat dStart dEnd hb (HeartbeatMatcher (Tuple genFunc desc)) =
    HeartbeatMatchResult $ Tuple signalMatch desc
    where
        generatedHb = generateHeartbeat dStart dEnd genFunc
        signalMatch = invertValue $ hammingDistanceWeighted generatedHb hb

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
            pure $ (Tuple startDate endDate)

-- TODO: More test cases for getMatcherResults
-- calculate the price that is being paid for a trend
-- create pipeline, split by merchant -> identify if trend is random process through autocorrelation-> find best match trend for each 
-- given a transaction does it belong to a trend from a given set of trends

-- need to either have matchers for every combination of days in a week
-- or need to allow the top N matches through
-- this is exemplified by the train matcher only atm returning that the transaction occurrs every tuesday
