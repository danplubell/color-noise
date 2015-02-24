module GennoiseSpec (spec) where

import Gennoise

import Test.Hspec

spec :: Spec
spec =
    describe "main" $ do
        it "returns the unit" $
            main `shouldReturn` ()
