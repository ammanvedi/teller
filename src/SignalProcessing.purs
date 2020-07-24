module SignalProcessing where

import Prelude

import Data.Array (filter, index, last, length, mapWithIndex, slice, sort)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

-- Correlation

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

stdDev :: Array Number -> Number -> Number
stdDev [] mean = 0.0
stdDev xs mean = minusMeanSum / (toNumber $ length xs)
    where 
        minusMeans = map (\ xi -> ((xi - mean) * (xi - mean))) xs
        minusMeanSum = sum minusMeans

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


-- Peak Detection

max :: Array Number -> Number
max xs =
    case lastItem of
        (Just i) -> i
        Nothing -> 0.0
    where
        sorted = sort xs
        lastItem = last sorted

peakFunction :: Int -> Int -> Number -> Array Number -> Number
peakFunction windowSize i xi xs = (backMax + forwardMax) / 2.0
    where
        backwardValues = slice (i - windowSize) i xs
        forwardValues = slice (i + 1) (i + windowSize + 1) xs
        backMinusXi = map (\ b -> xi - b) backwardValues
        forwardMinusXi = map (\ b -> xi - b) forwardValues
        backMax = max backMinusXi
        forwardMax = max forwardMinusXi

findPeaks :: Array Number -> Int -> Array Number
findPeaks xs windowSize = []
    where
        peakFnValues = mapWithIndex (\ ix val -> peakFunction windowSize ix val xs ) xs
        positivePeakFnValues = filter (\ xi -> xi >= 0.0) peakFnValues
        meanPeakFnValues = seriesMean positivePeakFnValues
        stdDevPeakFnValues = stdDev positivePeakFnValues meanPeakFnValues