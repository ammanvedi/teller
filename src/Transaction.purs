module Transaction where

import Prelude

import Data.List (List)
import Data.Number.Format
import Data.Int
import Data.Ordering
import Data.Ord
import Data.Date
import Data.Enum
import Data.Maybe

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

getStartOfMonth :: Date -> Maybe Date
getStartOfMonth nowDate = do
    year <- Just $ year nowDate
    month <- Just $ month nowDate
    day <- toEnum 1
    pure $ canonicalDate year month day
            

maybeDateToString :: Maybe Date -> String
maybeDateToString d = 
    case d of 
        (Just d) -> show d
        Nothing -> "NODATE"