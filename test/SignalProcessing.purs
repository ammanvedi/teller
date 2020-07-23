module Test.SignalProcessing where

import Data.Date

import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import DateHelpers (getStartOfMonth, getXDaysPrior)
import Prelude (Unit, bind, bottom, discard, negate, pure, ($))
import SignalProcessing as Test
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import SignalProcessing (autoCorrelation)


signalProcessingSpec :: Spec Unit
signalProcessingSpec =
    describe "autoCorrelation" do
        it "calculates the correct autocorrelation for a series" do
            let res = autoCorrelation [
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0
                ]
            res `shouldEqual` [1.0]