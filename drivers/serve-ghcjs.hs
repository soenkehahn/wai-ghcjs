{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified GHC.Generics
import           Network.Wai.Handler.Warp hiding (run)
import           System.IO
import           WithCli

import           Network.Wai.Shake.Ghcjs

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
  deriving (Show, GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options
instance HasArguments Options

run :: Options -> IO ()
run o@Options{..} = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on " ++ show port ++ "...")) $
        defaultSettings
  runSettings settings $ serveGhcjs mainIs sourceDirs
