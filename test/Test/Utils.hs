{-# LANGUAGE LambdaCase #-}

module Test.Utils where

import           Control.Exception
import           System.Environment.Compat

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
