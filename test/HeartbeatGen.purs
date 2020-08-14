module Test.HeartbeatGen where

import Data.Date (Date, Weekday(..), canonicalDate)
import Data.Date.Component (Weekday(..))
import Data.Enum (toEnum)
import Data.Maybe (fromMaybe)
import Data.Teller.HeartbeatGen (createAllBinaryPermutationsOfLength, genEveryDay, genLastWeekDay, genWeekday, genWeekend, genXthDayOfMonth, generateHeartbeat, getOccurringWeekdays, numberToPaddedBinaryString, padStringLeftWith)
import Prelude (Unit, bottom, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testStart :: Date
testStart = canonicalDate
    (fromMaybe bottom (toEnum 2020))
    (fromMaybe bottom (toEnum 7))
    (fromMaybe bottom (toEnum 4))

testEnd :: Date
testEnd = canonicalDate
    (fromMaybe bottom (toEnum 2020))
    (fromMaybe bottom (toEnum 7))
    (fromMaybe bottom (toEnum 14))

testStartLastX :: Date
testStartLastX = canonicalDate
    (fromMaybe bottom (toEnum 2020))
    (fromMaybe bottom (toEnum 6))
    (fromMaybe bottom (toEnum 24))

testEndLastX :: Date
testEndLastX = canonicalDate
    (fromMaybe bottom (toEnum 2020))
    (fromMaybe bottom (toEnum 8))
    (fromMaybe bottom (toEnum 1))

heartbeatGenSpec :: Spec Unit
heartbeatGenSpec =
    describe "Heartbeat Generator" do
        describe "getOccurringWeekdays" do
            it "should return the correct occurring weekdays" do
              let res = getOccurringWeekdays [0, 1, 0, 1, 0, 1, 1]
              res `shouldEqual` [Tuesday, Thursday, Saturday, Sunday]
        describe "createAllBinaryPermutationsOfLength" do
            it "should return the correct set of permuations of binary length 2" do
                let res = createAllBinaryPermutationsOfLength 2
                res `shouldEqual` [[0,1],[1,0],[1,1]]
            it "should return the correct set of permuations of binary length 3" do
                let res = createAllBinaryPermutationsOfLength 3
                res `shouldEqual` [[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
        describe "numberToPaddedBinaryString" do
            it "returns the correct padded string for 0" do
                let res = numberToPaddedBinaryString 0 7
                res `shouldEqual` "0000000"
            it "returns the correct padded string for 30" do
                let res = numberToPaddedBinaryString 30 7
                res `shouldEqual` "0011110"
        describe "padStringLeftWith" do
            it "Returns correctly padded value" do
                let res = padStringLeftWith "101" "0" 7
                res `shouldEqual` "0000101"
        describe "Every Day Heartbeat" do
            it "Returns the correct heartbeat every day" do
                let res = generateHeartbeat testStart testEnd genEveryDay
                res `shouldEqual` [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        describe "Week Day Heartbeat" do
            it "Returns the correct heartbeat on a week day" do
                let res = generateHeartbeat testStart testEnd genWeekday
                res `shouldEqual` [0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1]
        describe "Weekend Heartbeat" do
            it "Returns the correct heartbeat on the weekend" do
                let res = generateHeartbeat testStart testEnd genWeekend
                res `shouldEqual` [1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0]
        describe "Last Weekday" do
            it "Returns the correct heartbeat for last Friday of the month" do
                let res = generateHeartbeat testStartLastX testEndLastX (genLastWeekDay Friday)
                res `shouldEqual` [
                    0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 1, 0 ]
        describe "Xth Day Of Month" do
            it "Returns the correct heartbeat for first day of the month" do
                let res = generateHeartbeat testStartLastX testEndLastX (genXthDayOfMonth 1)
                res `shouldEqual` [
                    0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1 ]