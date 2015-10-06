{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Wai.Shake.GhcjsSpec where

import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.String.Conversions
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Network.Wai.Test
import           System.IO.Silently
import           System.IO.Temp
import           System.Process
import           Test.Hspec
import           Test.Hspec.Wai hiding (pending)
import           Test.Hspec.Wai.Internal
import           Test.Mockery.Directory
import           Test.QuickCheck

import           Network.Wai.Shake.Ghcjs

spec :: Spec
spec = do
  describe "serveGhcjs" $ do
    let mkCode :: String -> String
        mkCode msg = [i|
          main = putStrLn "#{msg}"
        |]
        config = BuildConfig "Main.hs" [] "build"
    it "serves html on /" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ mkCode "foo"
        app <- serveGhcjs config
        flip runWaiSession app $ do
          get "/" `shouldRespondWith` 200 {
            matchHeaders = ["Content-Type" <:> "text/html; charset=utf-8"]
          }

    it "serves the generated index.html on /" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ mkCode "foo"
        app <- serveGhcjs config
        flip runWaiSession app $ do
          output :: String <- cs <$> simpleBody <$> get "/"
          liftIO $ output `shouldContain`
            "<script language=\"javascript\" src=\"runmain.js\" defer></script>"

    it "serves javascript" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ mkCode "foo"
        app <- serveGhcjs config
        flip runWaiSession app $ do
          forM_ ["all.js", "rts.js", "lib.js", "out.js", "runmain.js"] $ \ file -> do
            get ("/" <> file) `shouldRespondWith` 200 {
              matchHeaders = ["Content-Type" <:> "application/javascript; charset=utf-8"]
            }

    it "compiles haskell files to javascript" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ mkCode "foo"
        app <- serveGhcjs config
        flip runWaiSession app $ do
          output <- getAndExecuteJs "all.js"
          liftIO (output `shouldBe` "foo\n")

    it "recompiles changed haskell sources" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ mkCode "foo"
        app <- serveGhcjs config
        flip runWaiSession app $ do
          _ <- getAndExecuteJs "all.js"
          liftIO $ writeFile "Main.hs" $ mkCode "bar"
          output <- getAndExecuteJs "all.js"
          liftIO (output `shouldBe` "bar\n")

    it "allows to have multiple modules" $ do
      inTempDirectory $ do
        writeFile "Lib.hs" $ unindent [i|
          module Lib where
          text = "foo"
        |]
        writeFile "Main.hs" $ unindent [i|
          import Lib
          main = putStrLn text
        |]
        app <- serveGhcjs config
        flip runWaiSession app $ do
          output <- getAndExecuteJs "all.js"
          liftIO (output `shouldBe` "foo\n")

    it "puts all compilation results in the given build dir" $ do
      withSystemTempDirectory "wai-shake" $ \ tmpDir -> do
        inTempDirectory $ do
          writeFile "Main.hs" $ mkCode "foo"
          app <- serveGhcjs config{buildDir = tmpDir}
          flip runWaiSession app $ do
            let listFiles = words <$> (liftIO $ capture_ $ callCommand "find")
            before <- listFiles
            _ <- get "/"
            after <- listFiles
            liftIO $ after `shouldMatchList` before

    it "overwrites index.html" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ unindent [i|
          main = putStrLn True
        |]
        app <- serveGhcjs config
        flip runWaiSession app $ do
          _ <- get "/"
          liftIO $ writeFile "Main.hs" $ unindent [i|
            main = putStrLn "foo"
          |]
          c :: String <- cs <$> simpleBody <$> get "/"
          liftIO $ c `shouldNotContain` "outputErrors.js"

    context "when used with invalid haskell files" $ do
      let writeInvalidMain = writeFile "Main.hs" $ unindent [i|
            main = putStrLn True
          |]
          err = "Couldn't match type ‘Bool’ with ‘[Char]’"
      it "outputs compiler errors to the javascript console" $ do
        inTempDirectory $ do
          writeInvalidMain
          app <- serveGhcjs config
          flip runWaiSession app $ do
            indexHtml :: String <- cs <$> simpleBody <$> get "/"
            liftIO $ indexHtml `shouldContain`
              "<script language=\"javascript\" src=\"outputErrors.js\" defer></script>"
            output <- getAndExecuteJs "outputErrors.js"
            liftIO $ output `shouldContain` err

      it "serves an index.html containing the error" $ do
        inTempDirectory $ do
          writeInvalidMain
          app <- serveGhcjs config
          flip runWaiSession app $ do
            output :: String <- cs <$> simpleBody <$> get "/"
            liftIO $ output `shouldContain` err

  describe "createJsToConsole" $ do
    it "creates a js file that outputs the given string" $ do
      property $ forAllShrink
        (listOf (suchThat arbitrary isPrint))
        (shrinkValidList isPrint) $
          \ ((++ "\n") -> s) ->
            inTempDirectory $ do
              LBS.writeFile "test.js" (createJsToConsole s)
              output <- capture_ $ callCommand "node test.js"
              output `shouldBe` s

getAndExecuteJs :: String -> WaiSession String
getAndExecuteJs urlPath = do
  js <- get ("/" <> cs urlPath)
  liftIO $ inTempDirectory $ do
    LBS.writeFile "main.js" $ simpleBody js
    capture_ (callCommand "node main.js")

shrinkValidList :: Arbitrary a => (a -> Bool) -> [a] -> [[a]]
shrinkValidList p l =
  filter (all p) $
  shrinkList shrink l
