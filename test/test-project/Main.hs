{-# LANGUAGE TemplateHaskell #-}

import           Network.Socket
import           Network.Wai.Ghcjs
import           Network.Wai.Handler.Warp
import           System.IO

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

openFreePort :: IO (Port, Socket)
openFreePort = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
