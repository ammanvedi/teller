module SignalProcessing where

import Prelude

import Data.Array (filter, index, last, length, mapWithIndex, slice, sort, (<>))
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd, fst)

newtype IndexedTuple = IndexedTuple (Tuple Int Number)

instance indexedTupleEq :: Eq IndexedTuple where
    eq (IndexedTuple ta) (IndexedTuple tb) = (snd ta) == (snd tb)

instance indexedTupleOrd :: Ord IndexedTuple where
    compare (IndexedTuple ta) (IndexedTuple tb) 
        | (snd ta) > (snd tb) = GT
        | (snd ta) < (snd tb) = LT
        | otherwise = EQ

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

max :: forall a. Ord a => Array a -> Maybe a
max xs = last $ sort xs

checkPair :: IndexedTuple -> Maybe IndexedTuple -> Int -> Array IndexedTuple
checkPair ta Nothing _ = [ta]
checkPair (IndexedTuple ta) (Just (IndexedTuple tb) ) windowSize =
    if violatesWindow
        then case max reWrapped of
            Nothing -> []
            Just t -> [t]
        else reWrapped
    where
        reWrapped = [IndexedTuple ta, IndexedTuple tb]
        violatesWindow = ((fst tb) - (fst ta)) <= windowSize

clampToWindow :: Array IndexedTuple -> Array IndexedTuple
clampToWindow = foldl (\ arr ta ->  )

peakFunction :: Int -> Number -> Int -> Number -> Array Number -> Number
peakFunction windowSize h i xi xs = (backMax + forwardMax) / 2.0
    where
        backwardValues = slice (i - windowSize) i xs
        forwardValues = slice (i + 1) (i + windowSize + 1) xs
        backMinusXi = map (\ b -> xi - b) backwardValues
        forwardMinusXi = map (\ b -> xi - b) forwardValues
        backMax = fromMaybe 0.0 $ max backMinusXi
        forwardMax = fromMaybe 0.0 $ max forwardMinusXi

removeSmallPeaks :: Array IndexedTuple -> Number -> Number -> Number -> Array IndexedTuple
removeSmallPeaks xs h m sd = filter (\ (IndexedTuple xi) -> (snd xi - m) > (h * sd)) xs

pairWithIndex :: Array Number -> Array IndexedTuple
pairWithIndex xs = mapWithIndex (\ i xi -> IndexedTuple (Tuple i xi)) xs

unPair :: Array IndexedTuple -> Array Number
unPair xs = map (\ (IndexedTuple xi) -> snd xi) xs

findPeaks :: Array Number -> Int -> Number -> Array Number
findPeaks xs windowSize h = []
    where
        peakFnValues = mapWithIndex (\ ix val -> peakFunction windowSize h ix val xs ) xs
        pairedFnValues = pairWithIndex peakFnValues
        positivePeakFnValues = filter (\ (IndexedTuple xi) -> snd xi > 0.0) pairedFnValues
        meanPeakFnValues = seriesMean $ unPair positivePeakFnValues
        stdDevPeakFnValues = stdDev (unPair positivePeakFnValues) meanPeakFnValues
        largePeaks = removeSmallPeaks positivePeakFnValues h meanPeakFnValues stdDevPeakFnValues