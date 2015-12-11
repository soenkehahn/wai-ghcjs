{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Wai.GhcjsSpec where

import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           System.IO.Silently
import           System.Process
import           Test.Hspec
import           Test.Mockery.Directory
import           Test.QuickCheck

import           Network.Wai.Ghcjs.Internal

spec :: Spec
spec = do
  describe "createJsToConsole" $ do
    it "creates a js file that outputs the given string" $ do
      property $ forAllShrink
        (listOf (suchThat arbitrary isPrint))
        (shrinkValidList isPrint) $
          \ ((++ "\n") -> s) ->
            inTempDirectory $ do
              pending
              LBS.writeFile "test.js" (createJsToConsole s)
              output <- capture_ $ callCommand "node test.js"
              output `shouldBe` s

shrinkValidList :: Arbitrary a => (a -> Bool) -> [a] -> [[a]]
shrinkValidList p l =
  filter (all p) $
  shrinkList shrink l
