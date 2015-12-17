{-# LANGUAGE DeriveAnyClass #-}

module Network.Wai.Ghcjs.Compiler where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default ()
import           Data.Foldable
import           Development.Shake as Shake
import           System.Directory
import           System.Environment
import           System.FilePath

import           Network.Wai.Ghcjs.Internal

data Paths
  = Paths {
    jsExeDir :: FilePath,
    shakeDir :: FilePath,
    targetIndexFile :: FilePath
  }
  deriving (Eq, Show)

withPaths :: BuildConfig -> (Paths -> IO a) -> IO a
withPaths config action = inCurrentDirectory (projectDir config) $ do
  let jsExeDir = buildDir config </> takeBaseName (mainFile config) <.> "jsexe"
      paths = Paths {
        jsExeDir = jsExeDir,
        shakeDir = buildDir config </> "shake",
        targetIndexFile = jsExeDir </> "index.html"
      }
  action paths

data Compiler
  = Compiler (BuildConfig -> Paths -> IO Result)

data Result
  = Success
  | ErrorIndex

runCompiler :: Compiler -> BuildConfig -> IO (FilePath, [FilePath])
runCompiler (Compiler compiler) config = withPaths config $ \ paths -> do
  let options = shakeOptions{
        shakeFiles = shakeDir paths
      }
  haskellFiles <- findHaskellFiles (getSourceDirs config)
  withArgs [] $ shakeArgs options $ do
    want [targetIndexFile paths]
    targetIndexFile paths %> \ _ -> do
      need haskellFiles
      result <- liftIO $ compiler config paths
      case result of
        Success -> do
          forM_ (customIndexFile config) $ \ customIndex -> do
            copyFileChanged customIndex (targetIndexFile paths)
        ErrorIndex -> return ()
  dependentFiles <- mapM canonicalizePath
    (haskellFiles ++ toList (customIndexFile config))
  return (jsExeDir paths, dependentFiles)
