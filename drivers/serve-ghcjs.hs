{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Network.Wai.Handler.Warp hiding (run)
import           System.IO
import           System.IO.Temp
import           WithCli

import           Network.Wai.Ghcjs

main :: IO ()
main = withCliModified mods run
  where
    mods =
      AddShortOption "port" 'p' :
      AddShortOption "sourceDirs" 'i' :
      AddShortOption "mainIs" 'm' :
      []

data Options
  = Options {
    port :: Int,
    mainIs :: String,
    sourceDirs :: [FilePath]
  }
  deriving (Show, Generic, HasArguments)

run :: Options -> IO ()
run Options{..} = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on " ++ show port ++ "...")) $
        defaultSettings
  withSystemTempDirectory "serve-ghcjs" $ \ buildDir -> do
    app <- mkDevelopmentApp (BuildConfig mainIs Nothing sourceDirs "." Cabal buildDir)
    runSettings settings app
