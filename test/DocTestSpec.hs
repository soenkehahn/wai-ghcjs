
module DocTestSpec where

import           Test.DocTest
import           Test.Hspec

spec :: Spec
spec = describe "doctests" $ do
  it "doctests" $ do
    doctest (words "src/Network/Wai/Shake/Ghcjs.hs -isrc")
