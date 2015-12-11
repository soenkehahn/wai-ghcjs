{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Ghcjs.DevelopmentSpec where

import qualified Codec.Compression.GZip (decompress)
import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable
import           Data.String.Conversions
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Wai.Test
import           System.Directory
import           System.Environment.Compat
import           System.FilePath
import           System.IO.Silently
import           System.IO.Temp
import           System.Posix.Files
import           System.Process
import           Test.Hspec
import           Test.Hspec.Wai hiding (pending)
import           Test.Hspec.Wai.Internal
import           Test.Mockery.Directory

import           Network.Wai.Ghcjs.Compiler
import           Network.Wai.Ghcjs.Development
import           Network.Wai.Ghcjs.Internal

spec :: Spec
spec = do

  let config = BuildConfig "Main.hs" Nothing [] "." Vanilla "tmp-build"

  describe "mkDevelopmentApp" $ do
    let mkCode :: String -> String
        mkCode msg = "main = putStrLn \"" ++ msg ++ "\""
        compiler = Compiler $ \ _config paths -> do
          code <- readFile "Main.hs"
          writeFile (jsExeDir paths </> "index.html") ("generated index:\n" ++ code)
          writeFile (jsExeDir paths </> "all.js") "generated javascript"
          touch "here"
          return Success

    it "serves html on /" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ mkCode "foo"
        app <- mkDevelopmentAppInternal compiler config
        flip runWaiSession app $ do
          get "/" `shouldRespondWith` 200 {
            matchHeaders = ["Content-Type" <:> "text/html; charset=utf-8"]
          }

    it "executes the compiler in the project directory" $ do
      inTempDirectory $ do
        touch "bar/Main.hs"
        writeFile "bar/Main.hs" $ mkCode "foo"
        app <- mkDevelopmentAppInternal compiler config{
          projectDir = "./bar"
        }
        flip runWaiSession app $ do
          get "/" `shouldRespondWith` 200
          liftIO $ doesFileExist "./bar/here" `shouldReturn` True

    forM_ ["/", "/index.html"] $ \ path -> do
      it ("serves the generated index.html on " ++ path) $ do
        inTempDirectory $ do
          writeFile "Main.hs" $ mkCode "foo"
          app <- mkDevelopmentAppInternal compiler config
          flip runWaiSession app $ do
            output :: String <- cs <$> simpleBody <$> get (cs path)
            liftIO $ head (lines output) `shouldBe` "generated index:"

    it "serves javascript" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ mkCode "foo"
        app <- mkDevelopmentAppInternal compiler config
        flip runWaiSession app $ do
          forM_ ["all.js"] $ \ file -> do
            get ("/" <> file) `shouldRespondWith` 200 {
              matchHeaders = ["Content-Type" <:> "application/javascript; charset=utf-8"]
            }

    it "overwrites index.html" $ do
      tryOften $ do
        inTempDirectory $ do
          writeFile "Main.hs" $
            "main = putStrLn True"
          makeOlder "Main.hs"
          app <- mkDevelopmentAppInternal compiler config
          flip runWaiSession app $ do
            _ <- get "/"
            liftIO $ writeFile "Main.hs" $
              "main = putStrLn \"foo\""
            c :: String <- cs <$> simpleBody <$> get "/"
            liftIO $ c `shouldNotContain` "True"

    context "when using custom index files" $ do
      it "serves them" $ do
        inTempDirectory $ do
          writeFile "Main.hs" "main file"
          writeFile "index.html" "custom index file"
          app <- mkDevelopmentAppInternal compiler config{
            customIndexFile = Just "index.html"
          }
          flip runWaiSession app $ do
            output :: String <- cs <$> decompress <$> simpleBody <$> get "/"
            liftIO $ output `shouldBe` "custom index file"

      it "reloads them for changes on disk correctly" $ do
        tryOften $ do
          inTempDirectory $ do
            writeFile "Main.hs" "main file"
            writeFile "index.html" "custom index file"
            makeOlder "index.html"
            app <- mkDevelopmentAppInternal compiler config{
              customIndexFile = Just "index.html"
            }
            flip runWaiSession app $ do
              _ <- get "/"
              liftIO $ writeFile "index.html" "new custom index file"
              output :: String <- cs <$> decompress <$> simpleBody <$> get "/"
              liftIO $ output `shouldBe` "new custom index file"

  describe "developmentCompiler" $ do
    it "generates an index.html that contains the error message" $ do
      withErrorGhcjs "ghcjs error message" $ do
        inTempDirectory $ do
          writeFile "Main.hs" "main = putStrLn True"
          let paths = Paths {
                jsExeDir = buildDir config </> "Main.jsexe",
                shakeDir = error "unused in test",
                targetIndexFile = error "unused in test"
              }
              (Compiler c) = developmentCompiler
          _ <- c config paths
          indexFile <- readFile (jsExeDir paths </> "index.html")
          indexFile `shouldContain` "ghcjs error message"

decompress :: LBS.ByteString -> LBS.ByteString
decompress input =
  if gzipMagicNumber `LBS.isPrefixOf` input
    then Codec.Compression.GZip.decompress input
    else input

gzipMagicNumber :: LBS.ByteString
gzipMagicNumber = LBS.pack [0x1f, 0x8b]

makeOlder :: FilePath -> IO ()
makeOlder file = do
  now <- getCurrentTime
  let older = utcTimeToPOSIXSeconds $ addUTCTime (- 5 * 60) now
  setFileTimesHiRes file older older

tryOften :: IO () -> IO ()
tryOften action = forM_ [1 .. 100 :: Int] $ \ _ -> silence action

withErrorGhcjs :: String -> IO a -> IO a
withErrorGhcjs message action = do
  withSystemTempDirectory "wai-ghcjs" $ \ tmpDir -> do
    let ghcjs = tmpDir </> "ghcjs"
    writeFile ghcjs $ unindent $ [i|
      #!/usr/bin/env bash
      echo #{message} > /dev/stderr
      exit 1
    |]
    callCommand ("chmod +x " ++ ghcjs)
    bracket (addPath tmpDir) removePath (const action)
  where
    addPath dir = do
      path <- getEnv "PATH"
      setEnv "PATH" (dir ++ ":" ++ path)
      return path
    removePath path = do
      setEnv "PATH" path
