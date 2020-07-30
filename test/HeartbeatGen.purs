module Test.HeartbeatGen where

import Data.Date (Date, canonicalDate)
import Data.Enum (toEnum)
import Data.Maybe (fromMaybe)
import HeartbeatGen (genEveryDay, genWeekday, generateHeartbeat)
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

heartbeatGenSpec :: Spec Unit
heartbeatGenSpec =
    describe "Heartbeat Generator" do
        describe "Every Day Heartbeat" do
            it "Returns the correct heartbeat" do
                let res = generateHeartbeat testStart testEnd genEveryDay
                res `shouldEqual` [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        describe "Week Day Heartbeat" do
            it "Returns the correct heartbeat" do
                let res = generateHeartbeat testStart testEnd genWeekday
                res `shouldEqual` [0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1]