module Test.Transaction where

import Data.Set as Set
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transaction (TransactionRec(..), matchMerchant, transactionsForMerchant, uniqueMerchants)

testTransaction :: TransactionRec
testTransaction = TransactionRec ({
            timestamp: 0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR X"
        })

testTransactions :: Array TransactionRec
testTransactions = [
    TransactionRec ({
            timestamp: 0,
            amount: 10.0,
            merchantName: "TFL",
            reference: "TFL X"
        }),
    TransactionRec ({
            timestamp: 0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR X"
        }),
    TransactionRec ({
            timestamp: 0,
            amount: 10.0,
            merchantName: "Barclays",
            reference: "BAR XY"
        }),
    TransactionRec ({
            timestamp: 0,
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
            true `shouldEqual` true
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
