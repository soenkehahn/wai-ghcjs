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

{-
  describe "serveGhcjs" $ do
    context "Production" $ do
      it "serves the generated index.html on /" $ do
        app <- $(serveGhcjs
          (BuildConfig "Main.hs" Nothing [] "test/resources/test-01" Vanilla
            "wai-shake-builds"))
          Production
        flip runWaiSession app $ do
          output :: String <- cs <$> decompress <$> simpleBody <$> get "/"
          liftIO $ output `shouldContain`
            "<script language=\"javascript\" src=\"runmain.js\" defer></script>" -}


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
