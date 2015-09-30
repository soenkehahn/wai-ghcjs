{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Wai.Shake.Ghcjs (
  serveGhcjs,
  BuildConfig(..),

  -- exported for testing:
  createJsToConsole,
 ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (mk)
import           Data.Default ()
import           Data.String.Conversions
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Development.Shake as Shake
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.CodeGen
import           Network.HTTP.Types
import           Network.Wai
import           System.Directory as System
import           System.Environment
import           System.Exit
import           System.FilePath

data BuildConfig = BuildConfig {
  mainFile :: FilePath
, sourceDirs :: [FilePath]
, buildDir :: FilePath
} deriving (Eq, Show)

serveGhcjs :: BuildConfig -> IO Application
serveGhcjs config = do
  mvar <- newMVar ()
  return $ app mvar config

app :: MVar () -> BuildConfig -> Application
app mvar config request respond = do
  createDirectoryIfMissing True (buildDir config)
  outDir <- ghcjsOrErrorToConsole mvar config
  case pathInfo request of
    [] -> serveFile "text/html" (outDir </> "index" <.> "html")
    [file] | ".js" == takeExtension (cs file) -> do
      fileExists <- System.doesFileExist (outDir </> cs file)
      if fileExists
        then serveFile "application/javascript" (outDir </> cs file)
        else send404
    _ -> send404
  where
    serveFile :: String -> FilePath -> IO ResponseReceived
    serveFile contentType file = do
      c <- LBS.readFile file
      respond $ responseLBS ok200
        [(mk (cs "Content-Type"), cs contentType <> cs "; charset=utf-8")] c

    send404 = respond $ responseLBS notFound404 [] (cs "404 - Not Found")

ghcjsOrErrorToConsole :: MVar () -> BuildConfig -> IO FilePath
ghcjsOrErrorToConsole mvar BuildConfig{mainFile, sourceDirs, buildDir} = do
  let outPattern = buildDir </> takeBaseName mainFile
      outDir = outPattern <.> "jsexe"
      indexFile = outDir </> "index.html"
      options = shakeOptions{
        shakeFiles = buildDir </> "shake"
      }
  forceToSingleThread mvar $ withArgs [] $ shakeArgs options $ do
    want [indexFile]
    indexFile %> \ outFile -> do
      need [mainFile]
      unit $ cmd "rm -f" outFile
      (Exit c, Stderr output) <- cmd "ghcjs -O0" mainFile "-o" outPattern
        (map ("-i" ++) sourceDirs)
        ("-outputdir=" ++ buildDir </> "output")
      when (c /= ExitSuccess) $ do
        liftIO $ createErrorPage outDir output
      return ()
  return outDir

createErrorPage :: FilePath -> String -> IO ()
createErrorPage dir msg = do
  let jsCode = createJsToConsole msg
  createDirectoryIfMissing True dir
  LBS.writeFile (dir </> "outputErrors.js") jsCode
  writeFile (dir </> "index.html") $ unindent [i|
    <!DOCTYPE html>
    <html>
      <head>
      </head>
      <body>
      </body>
      <script language="javascript" src="outputErrors.js" defer></script>
    </html>
  |]

createJsToConsole :: String -> LBS.ByteString
createJsToConsole msg =
  let escape :: String -> String
      escape s =
        show $ prettyPrint (string (doublePercentSigns s) :: Expression ())
      doublePercentSigns = concatMap (\ c -> if c == '%' then "%%" else [c])
  in cs $ unlines $ map (\ line -> "console.log(" ++ escape line ++ ");") (lines msg)

forceToSingleThread :: MVar () -> IO () -> IO ()
forceToSingleThread mvar action = modifyMVar mvar $ \ () -> do
  action
  return ((), ())
