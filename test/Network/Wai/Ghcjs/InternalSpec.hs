{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Wai.Ghcjs.InternalSpec where

import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Foldable
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           System.Directory
import           System.IO
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

  describe "readCompilationMode" $ do
    it "parses production" $ do
      withCompilationModeFile "production" $ do
        (fst <$> simulateCM readCompilationMode) `shouldReturn` Production

    it "parses development" $ do
      withCompilationModeFile "development\n" $ do
        (fst <$> simulateCM readCompilationMode) `shouldReturn` Development

    it "throws a helpful error on invalid files" $ do
      property $ \ t ->
        withCompilationModeFile t $ do
          simulateCM readCompilationMode `shouldThrow`
            (== ErrorCall ("invalid ghcjs-compilation-mode file:\n" ++ t))

    it "ignores comments" $ do
      withCompilationModeFile (unindent [i|
        # comment
        development
        # another comment
      |]) $ do
        (fst <$> simulateCM readCompilationMode) `shouldReturn` Development

    it "adds ./ghcjs-compilation-mode to the dependent files" $ do
      withCompilationModeFile "development" $ do
        file <- canonicalizePath compilationModeFile
        (snd <$> simulateCM readCompilationMode) `shouldReturn` [file]

    context "when ghcjs-compilation-mode doesn't exist" $ do
      it "creates a default file" $ do
        inTempDirectory $ do
          _ <- simulateCM readCompilationMode
          doesFileExist compilationModeFile `shouldReturn` True

      it "creates a parseable default file" $ do
        inTempDirectory $ do
          _ <- simulateCM readCompilationMode
          _ <- simulateCM readCompilationMode
          return ()

      it "creates a self-explanatory default file" $ do
        inTempDirectory $ do
          _ <- simulateCM readCompilationMode
          contents <- readFile compilationModeFile
          let keywords =
                "production" :
                "development" :
                "ghcjs" :
                "on the fly" :
                "embedded into the executable" :
                "template haskell" :
                []
          forM_ keywords $ \ w ->
            contents `shouldContain` w

      it "outputs a helpful message" $ do
        inTempDirectory $ do
          output <- hCapture_ [stderr] $ simulateCM readCompilationMode
          let keywords =
                "INFO" :
                "default" :
                compilationModeFile :
                []
          forM_ keywords $ \ w ->
            output `shouldContain` w

      it "adds ./ghcjs-compilation-mode to the dependent files" $ do
        inTempDirectory $ do
          dependents <- (snd <$> simulateCM readCompilationMode)
          file <- canonicalizePath compilationModeFile
          dependents `shouldBe` [file]

  describe "ifDevel" $ do
    it "returns the first argument when ./ghcjs-compilation-mode is set to 'development'" $ do
      withCompilationModeFile "development" $ do
        (fst <$> simulateCM (ifDevel "foo" "bar")) `shouldReturn` ("foo" :: String)

    it "returns the second argument when ./ghcjs-compilation-mode is set to 'production'" $ do
      withCompilationModeFile "production" $ do
        (fst <$> simulateCM (ifDevel "foo" "bar")) `shouldReturn` ("bar" :: String)

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

simulateCM :: CM a -> IO (a, [FilePath])
simulateCM = \ case
  (a :>>= b) -> do
    (x, files1) <- simulateCM a
    (y, files2) <- simulateCM (b x)
    return (y, files1 ++ files2)

  IO action -> (, []) <$> action
  AddDependentFile file -> return ((), [file])
