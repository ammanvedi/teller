module Test.Transaction where

import Data.Array as Array
import Data.Date (Month(..))
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transaction (TransactionRec(..), matchMerchant, transactionsForMerchant, uniqueMerchants, getTransactionMonth, transactionsOccurrInSuccessiveMonths)

testTransaction :: TransactionRec
testTransaction = TransactionRec ({
            timestamp: 1595361449000.0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR X"
        })

testTransactionBarclaysA :: TransactionRec
testTransactionBarclaysA = TransactionRec ({
            timestamp: 1582317957000.0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR X"
        })

testTransactionBarclaysB :: TransactionRec
testTransactionBarclaysB = TransactionRec ({
            timestamp: 1584823557000.0,
            amount: 100.0,
            merchantName: "Barclays",
            reference: "BAR XYZZ"
        })

testTransactions :: Array TransactionRec
testTransactions = [
    TransactionRec ({
            timestamp: 1579639557000.0,
            amount: 10.0,
            merchantName: "TFL",
            reference: "TFL X"
        }),
    testTransactionBarclaysA,
    testTransactionBarclaysB,
    TransactionRec ({
            timestamp: 1587501957000.0,
            amount: 10.0,
            merchantName: "Netflix",
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
    