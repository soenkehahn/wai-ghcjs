
module Network.Wai.Ghcjs.Development where

import           Control.Concurrent
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (mk)
import           Data.Default ()
import           Data.String.Conversions
import           Development.Shake
import           Network.HTTP.Types
import           Network.Wai
import           System.Directory as System
import           System.Exit
import           System.FilePath

import           Network.Wai.Ghcjs.Compiler
import           Network.Wai.Ghcjs.Internal

-- | Creates a wai 'Application' that serves a ghcjs executable (the client
-- application). Where to find
-- the executable and how to compile it is specified in the given 'BuildConfig'.
-- The 'Application' serves the created @index.html@ under both @/@ and
-- @/index.html@. All the needed javascript files that are referenced from the
-- @index.html@ are also served.
--
-- 'mkDevelopmentApp' compiles the Haskell sources on the fly. It will also
-- recompile them when they have changed on disk. In addition in case of compile
-- errors it will deliver an html page that contains the error messages.
--
-- This allows a workflow similar to those known from dynamic web programming
-- languages: You edit your code, save the file, switch to a browser and hit
-- reload. You will either see compilation errors in the browser or you will
-- get the newly compiled web application.
mkDevelopmentApp :: BuildConfig -> IO Application
mkDevelopmentApp = mkDevelopmentAppInternal developmentCompiler

mkDevelopmentAppInternal :: Compiler -> BuildConfig -> IO Application
mkDevelopmentAppInternal compiler userConfig = do
  config <- prepareConfig "development" userConfig
  mvar <- newMVar ()
  return $ developmentApp mvar compiler config

mkSimpleApp :: (Request -> IO Response) -> Application
mkSimpleApp app request respond = app request >>= respond

developmentApp :: MVar () -> Compiler -> BuildConfig -> Application
developmentApp mvar compiler config = mkSimpleApp $ \ request -> do
  (outDir, _) <- forceToSingleThread mvar $ do
    runCompiler compiler config
  let serveIndex = serveFile "text/html" (outDir </> "index" <.> "html")
  case pathInfo request of
    [] -> serveIndex
    [indexFile] | indexFile == cs "index.html" -> serveIndex
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

developmentCompiler :: Compiler
developmentCompiler = Compiler $ \ config paths -> do
  let output = dropExtension $ jsExeDir paths
  (Exit exitCode, Stderr output) <- cmd
    (addExec (projectExec config) "ghcjs") (mainFile config)
    "-o" output
    "-O0"
    (map ("-i" ++) (getSourceDirs config))
    ("-outputdir=" ++ buildDir config </> "output")
  case exitCode of
    ExitSuccess -> return Success
    ExitFailure _ -> do
      createErrorPage (jsExeDir paths) output
      return ErrorIndex

createErrorPage :: FilePath -> String -> IO ()
createErrorPage dir msg = do
  let jsCode = createJsToConsole msg
  createDirectoryIfMissing True dir
  LBS.writeFile (dir </> "outputErrors.js") jsCode -- fixme: remove
  writeFile (dir </> "index.html") $ unlines $
    "<!DOCTYPE html>" :
    "<html>" :
    "  <head>" :
    "  </head>" :
    "  <body>" :
    "    <pre>" :
    ("     " ++ msg) :
    "    </pre>" :
    "  </body>" :
    "  <script language=\"javascript\" src=\"outputErrors.js\" defer></script>" :
    "</html>" :
    []
