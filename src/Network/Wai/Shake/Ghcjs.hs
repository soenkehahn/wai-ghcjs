{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Shake.Ghcjs (
  mkDevelopmentApp,
  BuildConfig(..),
  Exec(..),

  mkProductionApp,

  serveGhcjs,
  Environment(..),
 ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (mk)
import           Data.Default ()
import           Data.String.Conversions
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

-- * development app

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
mkDevelopmentApp config = do
  mvar <- newMVar ()
  return $ developmentApp mvar config

mkSimpleApp :: (Request -> IO Response) -> Application
mkSimpleApp app request respond = app request >>= respond

developmentApp :: MVar () -> BuildConfig -> Application
developmentApp mvar config = mkSimpleApp $ \ request -> do
  outDir <- snd <$> forceToSingleThread mvar
    (ghcjsOrErrorToConsole config)
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

-- * production app

-- | 'mkProductionApp' is similar to 'mkDevelopmentApp' but it is meant for
-- use when you're not developing. It compiles the client application during
-- compilation of the server 'Application' using @TemplateHaskell@. It doesn't
-- recompile any files on changes. In addition it embeds the compilation results
-- into the executable, so it is completely self-contained. (I.e. you can scp
-- the executable to another server and it will be able to deliver the complete
-- client application.)
--
-- The spliced in fragment has type
--
-- @'IO' 'Application'@
--
-- >>> :set -XTemplateHaskell
-- >>> :type $(mkProductionApp (BuildConfig "Main.hs" [] "test/resources/test-01" Vanilla "wai-shake-builds"))
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(mkProductionApp (BuildConfig "Main.hs" [] "test/resources/test-01" Vanilla "wai-shake-builds"))
--   :: IO Application
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
  [|do
      (return () :: IO ())
      return $ staticApp
        ($(mkSettings (return embeddable))){
          ssIndices = [unsafeToPiece (cs ("index.html" :: String))]
        }|]
  where
    wrapWithMessages :: IO a -> IO a
    wrapWithMessages action = do
      hPutStrLn stderr "=====> building client code with ghcjs"
      result <- action
      hPutStrLn stderr "=====> done"
      return result

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

writeMVar :: MVar a -> a -> IO ()
writeMVar mvar a = modifyMVar_ mvar (const $ return a)

-- * serveGhcjs

-- | 'serveGhcjs' combines 'mkDevelopmentApp' and 'mkProductionApp'. It will
-- compile the client application during compilation of the server code. The
-- spliced in fragment has type
--
-- @'Environment' -> 'IO' 'Application'@
--
-- This
-- means that you can at runtime pass in an 'Environment' and either get the
-- behavior of 'mkProductionApp' or the one of 'mkDevelopmentApp', including
-- recompilation.
--
-- >>> :type $(serveGhcjs (BuildConfig "Main.hs" [] "test/resources/test-01" Vanilla "wai-shake-builds"))
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(serveGhcjs (BuildConfig "Main.hs" [] "test/resources/test-01" Vanilla "wai-shake-builds"))
--   :: Environment -> IO Application
--
-- So the 'BuildConfig' argument has to be supplied inside the @TemplateHaskell@
-- brackets while the 'Environment' argument has to be outside:
--
-- >>> :type $(serveGhcjs (BuildConfig "Main.hs" [] "test/resources/test-01" Vanilla "wai-shake-builds")) Development
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(serveGhcjs (BuildConfig "Main.hs" [] "test/resources/test-01" Vanilla "wai-shake-builds")) Development
--   :: IO Application
--
-- This way you can decide at runtime (e.g. depending on a command line flag)
-- whether to run in 'Development' or 'Production' mode.
serveGhcjs :: BuildConfig -> Q Exp
serveGhcjs config = [| \ env -> case env of
  Development -> mkDevelopmentApp config
  Production -> $(mkProductionApp config)|]
