{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Shake.Ghcjs (
  BuildConfig(..),
  Exec(..),
  Environment(..),
  serveGhcjs,
  mkDevelopmentApp,
  mkProductionApp,
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
import           Language.Haskell.TH
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           System.Directory as System
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           WaiAppStatic.Storage.Embedded
import           WaiAppStatic.Types

import           Network.Wai.Shake.Ghcjs.Embedded
import           Network.Wai.Shake.Ghcjs.Internal

serveGhcjs :: BuildConfig -> Q Exp
serveGhcjs config = [| \ env -> case env of
  Development -> mkDevelopmentApp config
  Production -> $(mkProductionApp config)|]

-- * production app

mkProductionApp :: BuildConfig -> Q Exp
mkProductionApp config = do
  embeddable <- runIO $ wrapWithMessages $ do
    (result, outDir) <- ghcjsOrErrorToConsole config
    case result of
      Failure err -> do
        callCommand ("rm -rf " ++ buildDir config)
        hPutStrLn stderr err
        die "ghcjs failed"
      Success -> mkSettingsFromDir outDir
  [|return $ staticApp
    ($(mkSettings (return embeddable))){
      ssIndices = [unsafeToPiece "index.html"]
    }|]
  where
    wrapWithMessages :: IO a -> IO a
    wrapWithMessages action = do
      hPutStrLn stderr "=====> building client code with ghcjs"
      result <- action
      hPutStrLn stderr "=====> done"
      return result

-- * development app

mkDevelopmentApp :: BuildConfig -> IO Application
mkDevelopmentApp config = do
  mvar <- newMVar ()
  return $ developmentApp mvar config

mkSimpleApp :: (Request -> IO Response) -> Application
mkSimpleApp app request respond = app request >>= respond

developmentApp :: MVar () -> BuildConfig -> Application
developmentApp mvar config = mkSimpleApp $ \ request -> do
  outDir <- snd <$> forceToSingleThread mvar
    (ghcjsOrErrorToConsole config)
  case pathInfo request of
    [] -> serveFile "text/html" (outDir </> "index" <.> "html")
    [file] | ".js" == takeExtension (cs file) -> do
      fileExists <- System.doesFileExist (outDir </> cs file)
      if fileExists
        then serveFile "application/javascript" (outDir </> cs file)
        else send404
    _ -> send404
  where
    serveFile :: String -> FilePath -> IO Response
    serveFile contentType file = do
      c <- LBS.readFile file
      return $ responseLBS ok200
        [(mk (cs "Content-Type"), cs contentType <> cs "; charset=utf-8")] c

    send404 = return $ responseLBS notFound404 [] (cs "404 - Not Found")

forceToSingleThread :: MVar () -> IO a -> IO a
forceToSingleThread mvar action = modifyMVar mvar $ \ () -> do
  a <- action
  return ((), a)

-- * ghcjs

data Result
  = Success
  | Failure String
  deriving (Eq, Show)

ghcjsOrErrorToConsole :: BuildConfig -> IO (Result, FilePath)
ghcjsOrErrorToConsole config = do
  createDirectoryIfMissing True (buildDir config)
  absBuildDir <- canonicalizePath (buildDir config)
  let outPattern = absBuildDir </> takeBaseName (mainFile config)
      outDir = outPattern <.> "jsexe"
      indexFile = outDir </> "index.html"
      options = shakeOptions{
        shakeFiles = absBuildDir </> "shake"
      }
  resultMVar <- newMVar Success
  withArgs [] $ shakeArgs options $ do
    want [indexFile]
    indexFile %> \ outFile -> do
      foundMainFile <- liftIO $ findMainFile config
      need [foundMainFile]
      unit $ cmd "rm -f" outFile
      (Exit c, Stderr output) <- cmd
        (Cwd (projectDir config))
        (addExec (projectExec config) "ghcjs -O0") (mainFile config)
        "-o" outPattern
        (map ("-i" ++) (sourceDirs config))
        ("-outputdir=" ++ absBuildDir </> "output")
      liftIO $ when (c /= ExitSuccess) $ do
        writeMVar resultMVar (Failure output)
        createErrorPage outDir output
      return ()
  result <- readMVar resultMVar
  return (result, outDir)

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
        <pre>
          #{msg}
        </pre>
      </body>
      <script language="javascript" src="outputErrors.js" defer></script>
    </html>
  |]

writeMVar :: MVar a -> a -> IO ()
writeMVar mvar a = modifyMVar_ mvar (const $ return a)
