module Data.Teller.SignalProcessing where

import Prelude

import Math (log, pow)
import Data.Array (filter, fromFoldable, head, index, last, length, mapWithIndex, slice, sort, zip)
import Data.Foldable (foldl, sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Set as Set
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

max' :: forall a. Ord a => Array a -> Maybe a
max' xs = last $ sort xs

checkPair :: IndexedTuple -> Maybe IndexedTuple -> Int -> Array IndexedTuple
checkPair ta Nothing _ = [ta]
checkPair (IndexedTuple ta) (Just (IndexedTuple tb) ) windowSize =
    if violatesWindow
        then case max' reWrapped of
            Nothing -> []
            Just t -> [t]
        else reWrapped
    where
        reWrapped = [IndexedTuple ta, IndexedTuple tb]
        violatesWindow = ((fst tb) - (fst ta)) <= windowSize

clampToWindow :: Array IndexedTuple -> Int -> Array IndexedTuple
clampToWindow xs windowSize = foldlWithIndex reducer [] xs
    where 
        reducer = (\ ix arr (IndexedTuple t) -> arr <> checkPair (IndexedTuple t) (index xs $ ix + 1) windowSize)

peakFunction :: Int -> Number -> Int -> Number -> Array Number -> Number
peakFunction windowSize h i xi xs = (backMax + forwardMax) / 2.0
    where
        backwardValues = slice (i - windowSize) i xs
        forwardValues = slice (i + 1) (i + windowSize + 1) xs
        backMinusXi = map (\ b -> xi - b) backwardValues
        forwardMinusXi = map (\ b -> xi - b) forwardValues
        backMax = fromMaybe 0.0 $ max' backMinusXi
        forwardMax = fromMaybe 0.0 $ max' forwardMinusXi

removeSmallPeaks :: Array IndexedTuple -> Number -> Number -> Number -> Array IndexedTuple
removeSmallPeaks xs h m sd = filter (\ (IndexedTuple xi) -> (snd xi - m) > (h * sd)) xs

pairWithIndex :: Array Number -> Array IndexedTuple
pairWithIndex xs = mapWithIndex (\ i xi -> IndexedTuple (Tuple i xi)) xs

unPair :: Array IndexedTuple -> Array Number
unPair xs = map (\ (IndexedTuple xi) -> snd xi) xs

findPeaks :: Array Number -> Int -> Number -> Array Int
findPeaks xs windowSize h = 
    fromFoldable $ Set.map (\ (IndexedTuple t) -> fst t) $ Set.fromFoldable clampedPeaks 
    where
        peakFnValues = mapWithIndex (\ ix val -> peakFunction windowSize h ix val xs ) xs
        pairedFnValues = pairWithIndex peakFnValues
        positivePeakFnValues = filter (\ (IndexedTuple xi) -> snd xi > 0.0) pairedFnValues
        meanPeakFnValues = seriesMean $ unPair positivePeakFnValues
        stdDevPeakFnValues = stdDev (unPair positivePeakFnValues) meanPeakFnValues
        largePeaks = removeSmallPeaks positivePeakFnValues h meanPeakFnValues stdDevPeakFnValues
        clampedPeaks = clampToWindow largePeaks windowSize

chunkConsec :: forall a. Array a -> Array (Array a)
chunkConsec xs = foldlWithIndex (\ i acc xi -> acc <> (safePair i xi)) [] xs
    where
        safePair :: Int -> a -> Array (Array a)
        safePair ix val = 
            case index xs (ix + 1) of
                (Just v) -> [[val, v]]
                Nothing -> []

diffChunks :: Array (Array Number) -> Array Number
diffChunks xs = map (\ arr -> abs $ (safeHead arr) - (safeTail arr)) xs
    where
        safeHead a = fromMaybe 0.0 $ head a
        safeTail a = fromMaybe 0.0 $ last a

averageDistance :: Array Number -> Number
averageDistance xs = seriesMean diffs
    where
        chunked = chunkConsec xs
        diffs = diffChunks chunked

getMatchingValues :: Array Int -> Array Int -> Int
getMatchingValues [] [] = 0
getMatchingValues _ [] = 0
getMatchingValues [] _ = 0
getMatchingValues x1 x2 = 
    foldl (\ acc (Tuple a b) -> 
        if a == b 
            then (acc + 1) 
            else acc
            ) 0 zipped
    where
        zipped = zip x1 x2

-- measure based on the percentage similarity between 
-- two binary values
naiveSignalMatch :: Array Int -> Array Int -> Number
naiveSignalMatch [] [] = 100.0
naiveSignalMatch _ [] = 0.0
naiveSignalMatch [] _ = 0.0
naiveSignalMatch x1 x2 = (valsThatMatch / listLeng) * 100.0
    where 
        valsThatMatch = toNumber $ getMatchingValues x1 x2
        listLeng = toNumber $ length x1

-- match based on how many positive points in first series
-- match the second series
hitCountMatch :: Array Int -> Array Int -> Number
hitCountMatch x y =
    toNumber $ foldl (\acc (Tuple a b) -> 
        if (a == 1 && b == 1) 
            then acc + 1 
            else acc 
        ) 0 zipped
    where
        zipped = zip x y



estimatePeriod :: Array Int -> Number
estimatePeriod xs =
    averageDistance $ map toNumber peaks
    where 
        ac = autoCorrelation $ map toNumber xs
        peaks = findPeaks ac 1 1.0

-- Operational Taxonomic Units (a b c d n)
data OTU = OTU Int Int Int Int Int

instance showOTU :: Show OTU where
    show (OTU a b c d n) = "a: " <> (show a) <> ", b: " <> (show b) <> ", c: " <> (show c) <> ", d: " <> (show d) <> ", n: " <> (show n)

instance eqOTU :: Eq OTU where
    eq (OTU ai bi ci di ni) (OTU aj bj cj dj nj) =
        ai == aj && bi == bj && ci == cj && di == dj && ni == nj

updateOTUForPosition :: Tuple Int Int -> OTU -> OTU
updateOTUForPosition (Tuple ji ii) (OTU a b c d n) 
    | ji == 1 && ii == 1 = (OTU (a + 1) b c d n)
    | ji == 1 && ii == 0 = (OTU a (b + 1) c d n)
    | ji == 0 && ii == 1 = (OTU a b (c + 1) d n)
    | ji == 0 && ii == 0 = (OTU a b c (d + 1) n)
    | otherwise = OTU a b c d n

-- http://www.iiisci.org/journal/CV$/sci/pdfs/GS315JG.pdf
calculateOTU :: Array Int -> Array Int -> OTU
calculateOTU a b = 
    foldl (\otu zipTuple -> updateOTUForPosition zipTuple otu) (OTU 0 0 0 0 (length a)) zippedSeq
    where
        zippedSeq = zip a b

gilbertAndWellsSimilarity :: Array Int -> Array Int -> Number
gilbertAndWellsSimilarity as bs =
    (log anum) - (log nnum) - (log ((anum + bnum) / nnum)) - (log ((anum + cnum) / nnum))
    where
        (OTU a b c d n) = calculateOTU as bs
        anum = toNumber a
        bnum = toNumber b
        cnum = toNumber c
        dnum = toNumber d
        nnum = toNumber n

e :: Number
e = 2.718

weightFunction :: Number -> Number -> Number -> Number -> Number
weightFunction wDelta n wMin x =
  linearTerm + expTerm
  where
    linearTerm = ((wDelta / n) * x) + wMin
    expTerm = pow e (x - n)

hammingDistanceWeighted :: Array Int -> Array Int -> Number
hammingDistanceWeighted xs ys = 
  foldlWithIndex (\i acc (Tuple a b) -> 
      if a /= b 
        then (acc + (1.0 * (weightFn $ toNumber i))) 
        else acc
    ) 0.0 (zip xs ys)
    where
      arrLen = toNumber $ length xs
      weightFn = weightFunction 0.25 arrLen 1.0

invertValue :: Number -> Number
invertValue 0.0 = 1.0
invertValue n = 1.0 / n