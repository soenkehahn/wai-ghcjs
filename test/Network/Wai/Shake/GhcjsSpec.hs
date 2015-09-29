{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Wai.Shake.GhcjsSpec where

import           Control.Monad
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.String.Conversions
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Network.Wai
import           Network.Wai.Test
import           System.Directory
import           System.FilePath
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
    let mkCode msg = [i|
          main = putStrLn "#{msg}"
        |]
    it "serves html on /" $ do
      inTempDirectory $ do
        createDirectoryIfMissing True "src"
        writeFile "src/Main.hs" $ mkCode "huhu"
        flip runWaiSession (serveGhcjs "src/Main.hs" [] "build") $ do
          get "/" `shouldRespondWith` 200 {
            matchHeaders = ["Content-Type" <:> "text/html; charset=utf-8"]
          }

    it "serves the generated index.html on /" $ do
      inTempDirectory $ do
        createDirectoryIfMissing True "src"
        writeFile "src/Main.hs" $ mkCode "huhu"
        flip runWaiSession (serveGhcjs "src/Main.hs" [] "build") $ do
          output :: String <- cs <$> simpleBody <$> get "/"
          liftIO $ output `shouldContain`
            "<script language=\"javascript\" src=\"runmain.js\" defer></script>"

    forM_ ["all.js", "rts.js", "lib.js", "out.js", "runmain.js"] $ \ file -> do
      it ("serves " ++ file) $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "src"
          writeFile "src/Main.hs" $ mkCode "huhu"
          flip runWaiSession (serveGhcjs "src/Main.hs" [] "build") $ do
            get ("/" <> cs file) `shouldRespondWith` 200 {
              matchHeaders = ["Content-Type" <:> "application/javascript; charset=utf-8"]
            }

    it "compiles haskell files to javascript" $ do
      inTempDirectory $ do
        createDirectoryIfMissing True "src"
        writeFile "src/Main.hs" $ mkCode "huhu"
        flip runWaiSession (serveGhcjs "src/Main.hs" [] "build") $ do
          output <- getAndExecuteJs "all.js"
          liftIO (output `shouldBe` "huhu\n")

    it "recompiles changed haskell sources" $ do
      inTempDirectory $ do
        createDirectoryIfMissing True "src"
        writeFile "src/Main.hs" $ mkCode "huhu"
        flip runWaiSession (serveGhcjs "src/Main.hs" [] "build") $ do
          output <- getAndExecuteJs "all.js"
          liftIO $ output `shouldBe` "huhu\n"
          liftIO $ writeFile "src/Main.hs" $ mkCode "foo"
          output <- getAndExecuteJs "all.js"
          liftIO (output `shouldBe` "foo\n")

    it "allows to have multiple modules" $ do
      inTempDirectory $ do
        createDirectoryIfMissing True "src"
        writeFile "src/Lib.hs" $ unindent [i|
          module Lib where
          text = "foo"
        |]
        writeFile "src/Main.hs" $ unindent [i|
          import Lib
          main = putStrLn text
        |]
        flip runWaiSession (serveGhcjs "src/Main.hs" ["src"] "build") $ do
          output <- getAndExecuteJs "all.js"
          liftIO (output `shouldBe` "foo\n")

    it "puts all compilation results in the given build dir" $ do
      withSystemTempDirectory "wai-shake" $ \ tmpDir -> do
        inTempDirectory $ do
          createDirectoryIfMissing True "src"
          writeFile "src/Main.hs" $ mkCode "huhu"
          flip runWaiSession (serveGhcjs "src/Main.hs" [] tmpDir) $ do
            let listFiles = words <$> (liftIO $ capture_ $ callCommand "find")
            before <- listFiles
            get "/"
            after <- listFiles
            liftIO $ after `shouldMatchList` before

    context "when used with invalid haskell files" $ do
      it "serves the normal index.html" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "src"
          writeFile "src/Main.hs" $ unindent [i|
            main = putStrLn True
          |]
          flip runWaiSession (serveGhcjs "src/Main.hs" ["src"] "build") $ do
            output :: String <- cs <$> simpleBody <$> get "/"
            liftIO $ output `shouldContain`
              "<script language=\"javascript\" src=\"runmain.js\" defer></script>"

      forM_ ["all.js", "runmain.js"] $ \ file -> do
        it ("outputs compiler errors to the javascript console in " ++ file) $ do
          inTempDirectory $ do
            createDirectoryIfMissing True "src"
            writeFile "src/Main.hs" $ unindent [i|
              main = putStrLn True
            |]
            flip runWaiSession (serveGhcjs "src/Main.hs" ["src"] "build") $ do
              output <- getAndExecuteJs file
              liftIO $ output `shouldContain`
                "Couldn't match type ‘Bool’ with ‘[Char]’"

  describe "createJsToConsole" $ do
    it "creates a js file that outputs the given string" $ do
      property $ forAllShrink (listOf (suchThat arbitrary isPrint)) (shrinkValidList isPrint) $
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
