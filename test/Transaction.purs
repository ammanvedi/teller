module Test.Transaction where

import Data.Array as Array
import Data.Date (Month(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transaction (TransactionRec(..), getBinaryHeartbeat, matchMerchant, transactionsForMerchant, uniqueMerchants, getTransactionMonth, transactionsOccurrInSuccessiveMonths, getHeartbeatChunk)

testTransaction :: TransactionRec
testTransaction = TransactionRec ({
            accountId: "acc",
            timestamp: 1595361449000.0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR X"
        })

testTransactionBarclaysA :: TransactionRec
testTransactionBarclaysA = TransactionRec ({
            accountId: "acc",
            timestamp: 1582317957000.0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR X"
        })

testTransactionBarclaysB :: TransactionRec
testTransactionBarclaysB = TransactionRec ({
            accountId: "acc",
            timestamp: 1584823557000.0,
            amount: 100.0,
            merchantName: "Barclays",
            reference: "BAR XYZZ"
        })

testTransactions :: Array TransactionRec
testTransactions = [
    TransactionRec ({
            accountId: "acc",
            timestamp: 1579639557000.0,
            amount: 10.0,
            merchantName: "TFL",
            reference: "TFL X"
        }),
    testTransactionBarclaysA,
    testTransactionBarclaysB,
    TransactionRec ({
            accountId: "acc",
            timestamp: 1587501957000.0,
            amount: 10.0,
            merchantName: "Netflix",
            reference: "BAR XY"
        })
]

testRangeTransactions :: Array TransactionRec
testRangeTransactions = [
    TransactionRec ({
            accountId: "acc",
            timestamp: 1595721600000.0, -- 26 jul
            amount: 10.0,
            merchantName: "TFL",
            reference: "TFL X"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1596585600000.0, -- 5 aug
            amount: 10.0,
            merchantName: "TFL",
            reference: "BAR XY"
        })
]

testRangeConsecTransactions :: Array TransactionRec
testRangeConsecTransactions = [
    TransactionRec ({
            accountId: "acc",
            timestamp: 1595721600000.0, -- 26 jul
            amount: 10.0,
            merchantName: "TFL",
            reference: "TFL X"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1595808000000.0, -- 27 jul
            amount: 10.0,
            merchantName: "TFL",
            reference: "BAR XY"
        })
]

hbTransactions :: Array TransactionRec
hbTransactions = [
    TransactionRec ({
            accountId: "acc",
            timestamp: 1595721600000.0, -- 26 jul
            amount: 10.0,
            merchantName: "TFL",
            reference: "TFL X"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1595808000000.0, -- 27 jul
            amount: 10.0,
            merchantName: "TFL",
            reference: "BAR XY"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1596758400000.0, -- 7 aug
            amount: 10.0,
            merchantName: "TFL",
            reference: "BAR XY"
        }),
    TransactionRec ({
            accountId: "acc",
            timestamp: 1596931200000.0, -- 9 aug
            amount: 10.0,
            merchantName: "TFL",
            reference: "BAR XY"
        })
]

transactionSpec :: Spec Unit
transactionSpec = 
  describe "Transaction" do
    describe "transactionsForMerchant" do
        it "Returns all transactions for a given merchant" do
            let res = transactionsForMerchant testTransactions "Barclays"
            (Array.length res) `shouldEqual` 2
            (Array.elem testTransactionBarclaysA res) `shouldEqual` true
            (Array.elem testTransactionBarclaysB res) `shouldEqual` true
    describe "matchMerchant" do
        it "Returns true for a matching merchant" do
            let res = matchMerchant "Barclays" testTransaction
            res `shouldEqual` true
        it "Returns false for a merchant that does not match" do
            let res = matchMerchant "Barclaysxyz" testTransaction
            res `shouldEqual` false
    describe "uniuqeMerchants" do
        it "Returns unique merchants with no duplicates" do
            let res = uniqueMerchants testTransactions
            Set.size res `shouldEqual` 3
            (Set.member "Barclays" res) `shouldEqual` true
            (Set.member "Netflix" res) `shouldEqual` true
            (Set.member "TFL" res) `shouldEqual` true
    describe "getTransactionMonth" do
        it "Returns the correct month" do
            let res = getTransactionMonth testTransaction
            let resUnwrap = fromMaybe January res
            resUnwrap `shouldEqual` July
    describe "transactionsOccurrInSuccessiveMonths" do
        it "returns true when transactions are in successive months" do
            let res = transactionsOccurrInSuccessiveMonths testTransactions
            res `shouldEqual` true
        it "returns false when transactions are not in successive months" do
            let res = transactionsOccurrInSuccessiveMonths [
                    testTransactionBarclaysB,
                    testTransactionBarclaysA
                ]
            res `shouldEqual` false
        it "returns false for a empty array" do
            let res = transactionsOccurrInSuccessiveMonths []
            res `shouldEqual` false
    describe "getHeartbeatChunk" do
        it "returns correct chunk for two dates" do
            let res = getHeartbeatChunk testRangeTransactions
            case res of
                (Just chunk) -> chunk `shouldEqual` [1,0,0,0,0,0,0,0,0,0]
                Nothing -> false `shouldEqual` true
        it "returns correct chunk for two dates on consecutive days" do
            let res = getHeartbeatChunk testRangeConsecTransactions
            case res of
                (Just chunk) -> chunk `shouldEqual` [1]
                Nothing -> false `shouldEqual` true
    describe "getBinaryHeartbeat" do
        it "calculates the correct heartbeat for a series of transactions" do
            let res = getBinaryHeartbeat hbTransactions
            res `shouldEqual` [1,1,0,0,0,0,0,0,0,0,0,0,1,0,1]