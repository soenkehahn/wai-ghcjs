{-# LANGUAGE TemplateHaskell #-}

import           Network.Wai.Ghcjs
import           Network.Wai.Handler.Warp
import           System.IO
import           Test.Hspec.Wai.Server

main :: IO ()
main = do
  (port, socket) <- openFreePort
  app <- $(serveGhcjs $ BuildConfig {
    mainFile = "Main.hs",
    customIndexFile = Nothing,
    sourceDirs = ["."],
    projectDir = "client",
    projectExec = Stack,
    buildDir = "builds"
  })
  let settings =
        setBeforeMainLoop (do
          print port
          hFlush stdout) $
        defaultSettings
  runSettingsSocket settings socket app
