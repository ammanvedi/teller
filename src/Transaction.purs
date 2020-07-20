module Transaction where

import Prelude
import Data.Array (filter, foldl)
import Data.Int (toNumber)
import Data.Number.Format (precision, toStringWith)
import Data.Set as Set

class Directional a where
    outgoing :: a -> Boolean

newtype TransactionRec =
    TransactionRec {
        timestamp :: Int,
        amount :: Number,
        merchantName :: String,
        reference :: String
    }

newtype TransactionList = TransactionList (Array TransactionRec)


testTransactionList :: TransactionRec
testTransactionList = TransactionRec ({
            timestamp: 0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR X"
        })

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
            transactionTime = toStringWith (precision 6) $ toNumber timestamp

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