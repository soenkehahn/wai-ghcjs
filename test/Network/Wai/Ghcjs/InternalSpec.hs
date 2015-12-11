
module Network.Wai.Ghcjs.InternalSpec where

import           Test.Hspec
import           Test.Mockery.Directory

import           Network.Wai.Ghcjs.Internal

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
