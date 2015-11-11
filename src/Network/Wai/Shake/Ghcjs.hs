{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Wai.Shake.Ghcjs (
  serveGhcjs,
  BuildConfig(..),
  Exec(..),

  -- exported for testing:
  createJsToConsole,
  findMainFile,
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
import           System.IO.Temp

data BuildConfig = BuildConfig {
  mainFile :: FilePath
, sourceDirs :: [FilePath]
, projectDir :: FilePath
, projectExec :: Exec
} deriving (Eq, Show)

getSourceDirs :: BuildConfig -> [FilePath]
getSourceDirs config = case sourceDirs config of
  [] -> ["."]
  dirs -> dirs

data Exec
  = Vanilla
  | Cabal
  | Stack
  deriving (Eq, Show)

addExec :: Exec -> String -> String
addExec exec command = case exec of
  Vanilla -> command
  Cabal -> "cabal exec -- " ++ command
  Stack -> "stack exec -- " ++ command

serveGhcjs :: BuildConfig -> IO Application
serveGhcjs config = do
  mvar <- newMVar ()
  withSystemTempDirectory "wai-shake" $ \ buildDir -> do
    return $ app mvar buildDir config

simpleApp :: (Request -> IO Response) -> Application
simpleApp app request respond = app request >>= respond

app :: MVar () -> FilePath -> BuildConfig -> Application
app mvar buildDir config = simpleApp $ \ request -> do
  createDirectoryIfMissing True buildDir
  outDir <- ghcjsOrErrorToConsole mvar buildDir config
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

ghcjsOrErrorToConsole :: MVar () -> FilePath -> BuildConfig -> IO FilePath
ghcjsOrErrorToConsole mvar buildDir config = do
  let outPattern = buildDir </> takeBaseName (mainFile config)
      outDir = outPattern <.> "jsexe"
      indexFile = outDir </> "index.html"
      options = shakeOptions{
        shakeFiles = buildDir </> "shake"
      }
  forceToSingleThread mvar $ withArgs [] $ shakeArgs options $ do
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
        ("-outputdir=" ++ buildDir </> "output")
      when (c /= ExitSuccess) $ do
        liftIO $ createErrorPage outDir output
      return ()
  return outDir

findMainFile :: BuildConfig -> IO FilePath
findMainFile config =
  lookup $ map
    (\ srcDir -> projectDir config </> srcDir </> mainFile config)
    (getSourceDirs config)
  where
    lookup :: [FilePath] -> IO FilePath
    lookup (a : r) = do
      exists <- System.doesFileExist a
      if exists
        then canonicalizePath a
        else lookup r
    lookup [] = die ("cannot find " ++ mainFile config)

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
