{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Wai.Ghcjs.InternalSpec where

import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           System.IO.Silently
import           System.Process
import           Test.Hspec
import           Test.Mockery.Directory
import           Test.QuickCheck

import           Network.Wai.Ghcjs.Internal
import           Test.Utils

spec :: Spec
spec = do
  describe "findHaskellFiles" $ do
    it "finds a haskell file" $ do
      inTempDirectory $ do
        touch "Foo.hs"
        findHaskellFiles ["."] `shouldReturn` ["Foo.hs"]

    it "ignores other files" $ do
      inTempDirectory $ do
        inTempDirectory $ do
          touch "Foo.c"
          findHaskellFiles ["."] `shouldReturn` []

    it "finds multiple haskell files" $ do
      inTempDirectory $ do
        touch "Foo.hs"
        touch "Bar.hs"
        findHaskellFiles ["."] `shouldReturn` ["Bar.hs", "Foo.hs"]

    it "finds haskell files recursively" $ do
      inTempDirectory $ do
        touch "foo/Bar.hs"
        findHaskellFiles ["."] `shouldReturn` ["foo/Bar.hs"]

    it "finds haskell files in source directories" $ do
      inTempDirectory $ do
        touch "src/Foo.hs"
        findHaskellFiles ["src"] `shouldReturn` ["src/Foo.hs"]

    it "doesn't return files twice" $ do
      inTempDirectory $ do
        touch "src/Foo.hs"
        findHaskellFiles [".", "src"] `shouldReturn` ["src/Foo.hs"]

    it "ignores hidden files" $ do
      inTempDirectory $ do
        touch ".Test.hs"
        findHaskellFiles ["."] `shouldReturn` []

    it "doesn't descend into hidden directories" $ do
      inTempDirectory $ do
        touch ".foo/Test.hs"
        findHaskellFiles ["."] `shouldReturn` []

    it "correctly returns files in the parent directory" $ do
      inTempDirectory $ do
        touch "project/foo"
        touch "Bar.hs"
        inCurrentDirectory "project" $ do
          findHaskellFiles [".."] `shouldReturn` ["../Bar.hs"]

    it "correctly returns files in sibling directories" $ do
      inTempDirectory $ do
        touch "project/Foo.hs"
        touch "sibling/Bar.hs"
        inCurrentDirectory "project" $ do
          findHaskellFiles ["../sibling"] `shouldReturn` ["../sibling/Bar.hs"]

  describe "ifDevel" $ do
    it "returns the first argument when DEVEL is set" $ do
      modifyEnvVar "DEVEL" (const $ Just "1") $
        ifDevel "foo" "bar" `shouldReturn` ("foo" :: String)

    it "returns the second argument when DEVEL is not set" $ do
      modifyEnvVar "DEVEL" (const Nothing) $
        ifDevel "foo" "bar" `shouldReturn` ("bar" :: String)

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
