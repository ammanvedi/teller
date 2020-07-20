module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, discard, ($))
import Test.Date (dateSpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)
import Test.Transaction (transactionSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [specReporter] do
    dateSpec
    transactionSpec
