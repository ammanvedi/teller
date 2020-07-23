module Transaction where

import Data.Maybe
import Prelude

import Data.Array (filter, foldl, head, reverse, sort, tail)
import Data.Date.Component (Month(..))
import Data.DateTime (month, date)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Enum (succ)
import Data.Int (toNumber)
import Data.Interval.Duration (millisecond)
import Data.JSDate (toInstant)
import Data.Number.Format (precision, toStringWith)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..), convertDuration, fromDuration)

class Directional a where
    outgoing :: a -> Boolean

newtype TransactionRec =
    TransactionRec {
        timestamp :: Number,
        amount :: Number,
        merchantName :: String,
        reference :: String
    }

newtype TransactionList = TransactionList (Array TransactionRec)

instance transactionDirectional :: Directional TransactionRec where
    outgoing (TransactionRec {amount}) = amount >= toNumber 0

instance transactionEq :: Eq TransactionRec where
    eq (TransactionRec {timestamp: tsa, amount: aa, merchantName: mna}) 
        (TransactionRec {timestamp: tsb, amount: ab, merchantName: mnb})
         = dateMatch && amountMatch && merchantNameMatch
            where dateMatch = tsa == tsb
                  amountMatch = aa == ab
                  merchantNameMatch = mna == mnb

instance transactionShow :: Show TransactionRec where
    show (TransactionRec {timestamp, amount, merchantName})
        = transactionTime <> " " <> merchantName <> " " <> transactionAmount
        where
            transactionAmount = toStringWith (precision 6) amount
            transactionTime = toStringWith (precision 6) $ timestamp

instance transactionOrd :: Ord TransactionRec where
    compare
        (TransactionRec {timestamp: ta})
        (TransactionRec {timestamp: tb})
            | ta > tb = GT
            | ta < tb = LT
            | otherwise = EQ


transactionsForMerchant :: (Array TransactionRec) -> String -> Array TransactionRec
transactionsForMerchant xs m = filter (matchMerchant m) xs

matchMerchant :: String -> TransactionRec -> Boolean
matchMerchant m (TransactionRec {merchantName}) = merchantName == m

uniqueMerchants :: Array TransactionRec -> Set.Set String
uniqueMerchants xs = 
            foldl (\accSet (TransactionRec t) -> Set.insert t.merchantName accSet) Set.empty xs

getTransactionMonth :: TransactionRec -> Maybe Month
getTransactionMonth (TransactionRec { timestamp }) =
    case tInstant of
        Just instant ->
            Just (month $ date $ toDateTime instant)
        Nothing -> Nothing
    where
        msDuration = Milliseconds $ timestamp
        tInstant = instant msDuration

transactionsOccurrInSuccessiveMonths :: Array TransactionRec -> Boolean
transactionsOccurrInSuccessiveMonths [] = false
transactionsOccurrInSuccessiveMonths [_] = true
transactionsOccurrInSuccessiveMonths xs =
    case chainHoldsToNextMonth of
        (Just holds) ->
            case holds of
                true -> transactionsOccurrInSuccessiveMonths unwrapTail
                false -> false
        Nothing -> false
    where
        unwrapTail = fromMaybe [] $ tail xs
        chainHoldsToNextMonth = do
            t1 <- head xs
            tailXs <- tail xs
            t2 <- head tailXs
            t1Month <- getTransactionMonth t1
            t2Month <- getTransactionMonth t2
            succT1 <- succ t1Month
            pure $ t2Month == succT1

sortTransactionsByDateDesc :: Array TransactionRec -> Array TransactionRec
sortTransactionsByDateDesc xs = sort xs

getOutgoingTransactions :: forall a. Directional a => Array a -> Array a
getOutgoingTransactions xs = filter outgoing xs