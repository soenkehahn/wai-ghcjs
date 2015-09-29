{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Wai.Shake.Ghcjs where

import           Control.Monad
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
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.Process

serveGhcjs :: FilePath -> [FilePath] -> FilePath -> Application
serveGhcjs mainFile sourceDirs buildDir request respond = respond =<< do
  createDirectoryIfMissing True buildDir
  outDir <- ghcjsOrErrorToConsole mainFile sourceDirs buildDir
  case pathInfo request of
    [] -> serveFile "text/html" (outDir </> "index" <.> "html")
    [file] | ".js" == takeExtension (cs file) -> do
      fileExists <- doesFileExist (outDir </> cs file)
      if fileExists
        then serveFile "application/javascript" (outDir </> cs file)
        else return $ responseLBS notFound404 [] "404 - Not Found"

ghcjsOrErrorToConsole :: FilePath -> [FilePath] -> FilePath -> IO FilePath
ghcjsOrErrorToConsole mainFile sourceDirs buildDir = do
  let outPattern = buildDir </> takeBaseName mainFile
      outDir = outPattern <.> "jsexe"
      sourceDirFlags = unwords $ map ("-i" ++) sourceDirs
  (exitCode, out, err) <- readProcessWithExitCode "ghcjs"
    ["-O0", mainFile, "-o", outPattern, sourceDirFlags, "-outputdir=" ++ buildDir </> "output"]
    ""
  case exitCode of
    ExitSuccess -> do
      return ()
    ExitFailure _ -> do
      createDirectoryIfMissing True outDir
      let jsCode = createJsToConsole (out ++ "\n" ++ err)
      LBS.writeFile (outDir </> "runmain.js") jsCode
      LBS.writeFile (outDir </> "all.js") jsCode
      writeFile (outDir </> "index.html") $ unindent [i|
        <!DOCTYPE html>
        <html>
          <head>
          </head>
          <body>
          </body>
          <script language="javascript" src="runmain.js" defer></script>
        </html>
      |]
  return outDir

createJsToConsole :: String -> LBS.ByteString
createJsToConsole msg =
  let escape :: String -> String
      escape s =
        show $ prettyPrint (string (doublePercentSigns s) :: Expression ())
      doublePercentSigns = concatMap (\ c -> if c == '%' then "%%" else [c])
  in cs $ unlines $ map (\ line -> "console.log(" ++ escape line ++ ");") (lines msg)

serveFile :: String -> FilePath -> IO Response
serveFile contentType file = do
  c <- LBS.readFile file
  return $ responseLBS ok200 [("Content-Type", cs contentType <> "; charset=utf-8")] c
