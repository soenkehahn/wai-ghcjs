{-# LANGUAGE LambdaCase #-}

module Test.Utils where

import qualified Codec.Compression.GZip (decompress)
import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import           System.Environment.Compat
import           Test.Mockery.Directory

import           Network.Wai.Ghcjs.Internal

modifyEnvVar :: String -> (Maybe String -> Maybe String) -> IO a -> IO a
modifyEnvVar var f =
  bracket (setUp f) tearDown . const
  where
    setUp f = do
      value <- lookupEnv var
      modEnvVar $ f value
      return value
    tearDown value = modEnvVar value
    modEnvVar = \ case
      Just new -> setEnv var new
      Nothing -> unsetEnv var

decompress :: LBS.ByteString -> LBS.ByteString
decompress input =
  if gzipMagicNumber `LBS.isPrefixOf` input
    then Codec.Compression.GZip.decompress input
    else input

gzipMagicNumber :: LBS.ByteString
gzipMagicNumber = LBS.pack [0x1f, 0x8b]

withCompilationModeFile :: String -> IO a -> IO a
withCompilationModeFile contents action = do
  inTempDirectory $ do
    writeFile compilationModeFile contents
    action
