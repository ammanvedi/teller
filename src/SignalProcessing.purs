module SignalProcessing where

import Prelude

import Data.Array (index, length, mapWithIndex, slice)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

atIndex :: Array Number -> Int -> Number
atIndex xs n = 
    case wrappedIndex of
        (Just ix) -> ix
        Nothing -> 0.0
    where 
        wrappedIndex = index xs n

seriesMean :: Array Number -> Number
seriesMean xs = seriesSum / seriesLen
    where
        seriesLen = toNumber $ length xs
        seriesSum = sum xs

varianceProduct :: Number -> Number -> Number -> Number
varianceProduct yi yj yMean = (yi - yMean) * (yj - yMean)

seriesVariances :: Int -> Number -> Array Number -> Array Number
seriesVariances k mean xs = 
    mapWithIndex (\ i yi -> varianceProduct yi (arrValAt $ i + k) mean ) remainderSeries
    where
        arrValAt = atIndex xs
        arrLen = length xs
        remainderSeries = slice 0 (arrLen - k) xs

autoCoVarianceAtK :: Int -> Number -> Array Number -> Number
autoCoVarianceAtK k mean xs = (sum variances) / toNumber (length xs)
    where
        variances = seriesVariances k mean xs

autoCorrelationAtLagK :: Int -> Array Number -> Number
autoCorrelationAtLagK k xs = coVarianceAtK / coVarianceAtZero
    where
        xsMean = seriesMean xs
        coVarianceAtZero = autoCoVarianceAtK 0 xsMean xs
        coVarianceAtK = autoCoVarianceAtK k xsMean xs

autoCorrelation :: Array Number -> Array Number
autoCorrelation xs = 
    mapWithIndex (\ i _ -> autoCorrelationAtLagK i xs) xs