module Transaction where

import Data.Date
import Data.Time.Duration
import Data.Enum
import Data.Int
import Data.Maybe
import Data.Number.Format
import Data.Ord
import Data.Ordering
import Prelude

import Data.List (List)
import Effect.Now (nowDate)

class Directional a where
    outgoing :: a -> Boolean

newtype Transaction =
    Transaction {
        timestamp :: Int,
        amount :: Number,
        merchantName :: String,
        reference :: String
    }

newtype TransactionList = TransactionList (Array Transaction)

instance transactionDirectional :: Directional Transaction where
    outgoing (Transaction {amount}) = amount >= toNumber 0

instance transactionEq :: Eq Transaction where
    eq (Transaction {timestamp: tsa, amount: aa, merchantName: mna}) 
        (Transaction {timestamp: tsb, amount: ab, merchantName: mnb})
         = dateMatch && amountMatch && merchantNameMatch
            where dateMatch = tsa == tsb
                  amountMatch = aa == ab
                  merchantNameMatch = mna == mnb

instance transactionShow :: Show Transaction where
    show (Transaction {timestamp, amount, merchantName})
        = transactionTime <> " " <> merchantName <> " " <> transactionAmount
        where
            transactionAmount = toStringWith (precision 6) amount
            transactionTime = toStringWith (precision 6) $ toNumber timestamp

instance transactionOrd :: Ord Transaction where
    compare
        (Transaction {timestamp: ta})
        (Transaction {timestamp: tb})
            | ta > tb = GT
            | ta < tb = LT
            | otherwise = EQ