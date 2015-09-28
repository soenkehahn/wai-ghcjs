{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Wai.Shake.Ghcjs where

import qualified Data.ByteString.Lazy as LBS
import           Data.Default
import           Data.String.Conversions
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.CodeGen
import           Network.HTTP.Types
import           Network.Wai
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.Process

serveGhcjs :: FilePath -> [FilePath] -> Application
serveGhcjs mainFile sourceDirs request respond = respond =<< do
  let outPattern = dropExtension mainFile
      outDir = outPattern <.> "jsexe"
      sourceDirFlags = unwords $ map ("-i" ++) sourceDirs
  initializeFiles outPattern
  (exitCode, out, err) <- readProcessWithExitCode "ghcjs"
    (words [i|-O0 #{mainFile} -o #{outPattern} #{sourceDirFlags}|])
    ""
  case pathInfo request of
    [] -> serveFile "text/html" (outDir </> "index" <.> "html")
    [file] | ".js" == takeExtension (cs file) ->
      case exitCode of
        ExitSuccess -> do
          serveFile "application/javascript" (outDir </> cs file)
        ExitFailure _ -> do
          return $ responseLBS ok200 [] (createJsToConsole (out ++ "\n" ++ err))
    _ -> return $ responseLBS notFound404 [] "404 - Not Found"

createJsToConsole :: String -> LBS.ByteString
createJsToConsole msg =
  let escaped :: String
      escaped = show $ prettyPrint (string msg :: Expression ())
  in cs $ unindent [i|
       console.log(#{escaped});
     |]

serveFile :: String -> FilePath -> IO Response
serveFile contentType file = do
  c <- LBS.readFile file
  return $ responseLBS ok200 [("Content-Type", cs contentType <> "; charset=utf-8")] c

initializeFiles :: String -> IO ()
initializeFiles outPattern = do
  withSystemTempDirectory "serve-ghcjs" $ \ dir -> do
    let mainFile = dir </> "Main.hs"
    writeFile mainFile [i|
      main = return ()
    |]
    (ExitSuccess, out, err) <- readProcessWithExitCode "ghcjs"
      (words [i|-O0 #{mainFile} -o #{outPattern}|])
      ""
    return ()
