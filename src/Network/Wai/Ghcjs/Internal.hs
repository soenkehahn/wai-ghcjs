{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Network.Wai.Ghcjs.Internal where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Default ()
import           Data.List
import           Data.String.Conversions
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.CodeGen
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Syntax
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO

-- | Specifies how to build the client application.
data BuildConfig = BuildConfig {
  mainFile :: FilePath
    -- ^ location of the main module
, customIndexFile :: Maybe FilePath
    -- ^ custom @index.html@ file
, sourceDirs :: [FilePath]
    -- ^ where to look for Haskell source files
, projectDir :: FilePath
    -- ^ where the client application resides. Both 'mainFile' and
    -- 'sourceDirs' are interpreted relative to 'projectDir'.
, projectExec :: Exec
    -- ^ which ghcjs package databases to use (see 'Exec')
, buildDir :: FilePath
    -- ^ where to store build results
} deriving (Eq, Show)

instance Lift BuildConfig where
  lift = \ case
    BuildConfig a b c d e f -> [|BuildConfig a b c d e f|]

getSourceDirs :: BuildConfig -> [FilePath]
getSourceDirs config = case sourceDirs config of
  [] -> ["."]
  dirs -> dirs

prepareConfig :: String -> BuildConfig -> IO BuildConfig
prepareConfig environment config = do
  let dir = buildDir config </> environment
  createDirectoryIfMissing True dir
  absBuildDir <- canonicalizePath dir
  return $ config {
    buildDir = absBuildDir
  }

-- | In case your client application needs dependencies that are
-- installed in a @cabal@ sandbox or through @stack@ you can specify
-- that with 'Exec'.
data Exec
  = Vanilla
    -- ^ no additional package databases are needed
  | Cabal
    -- ^ execute build commands prefixed with @cabal exec --@
  | Stack
    -- ^ execute build commands prefixed with @stack exec --@
  deriving (Eq, Show)

instance Lift Exec where
  lift = \ case
    Vanilla -> [|Vanilla|]
    Cabal -> [|Cabal|]
    Stack -> [|Stack|]

addExec :: Exec -> String -> String
addExec exec command = case exec of
  Vanilla -> command
  Cabal -> "cabal exec -- " ++ command
  Stack -> "stack exec -- " ++ command

findHaskellFiles :: MonadIO m => [FilePath] -> m [FilePath]
findHaskellFiles sourceDirs = liftIO $ do
 r <-
  sort <$>
  nub <$>
  map normalise <$>
  concat <$>
  map inner <$>
  mapM (readDirectoryWith (const $ return ())) sourceDirs
 return r
  where
    inner :: AnchoredDirTree () -> [FilePath]
    inner (anchor :/ dirTree) = map (anchor </>) $ case dirTree of
      File name () ->
        if isHaskellFile name && not (isHidden name)
          then [name]
          else []
      Dir name children ->
        if not (isHidden name)
          then concat $ map inner $ map (name :/) children
          else []
      Failed name err -> error $ show (name, err)
    isHaskellFile = (== ".hs") . takeExtension
    isHidden = \ case
      "." -> False
      ".." -> False
      '.' : _ -> True
      _ -> False

inCurrentDirectory :: FilePath -> IO a -> IO a
inCurrentDirectory dir action = bracket before after (const action)
  where
    before = do
      old <- getCurrentDirectory
      setCurrentDirectory dir
      return old
    after old = do
      setCurrentDirectory old

createJsToConsole :: String -> LBS.ByteString
createJsToConsole msg =
  let escape :: String -> String
      escape s =
        show $ prettyPrint (string (doublePercentSigns s) :: Expression ())
      doublePercentSigns = concatMap (\ c -> if c == '%' then "%%" else [c])
  in cs $ unlines $ map (\ line -> "console.log(" ++ escape line ++ ");") (lines msg)

ifDevel :: a -> a -> CM a
ifDevel a b = do
  mode <- readCompilationMode
  return $ case mode of
    Development -> a
    Production -> b

data CompilationMode
  = Production
  | Development
  deriving (Eq, Ord, Show)

compilationModeFile :: FilePath
compilationModeFile = "ghcjs-compilation-mode"

readCompilationMode :: CM CompilationMode
readCompilationMode = do
  file <- IO $ do
    createIfMissing
    canonicalizePath compilationModeFile
  AddDependentFile file
  IO $ do
    contents <- readFile file
    case parse contents of
      Right m -> return m
      Left () -> throwIO $ ErrorCall
        ("invalid " ++ compilationModeFile ++ " file:\n" ++ contents)
  where
    parse :: String -> Either () CompilationMode
    parse =
      lines >>>
      map (dropWhile isSpace) >>>
      filter (not . ("#" `isPrefixOf`)) >>>
      concatMap words >>>
      (\ case
        ["development"] -> return Development
        ["production"] -> return Production
        _ -> Left ())

createIfMissing :: IO ()
createIfMissing = do
  exists <- doesFileExist compilationModeFile
  when (not exists) $ do
    writeFile compilationModeFile $ unlines $
      "# This file controls the compilation mode for the client code through ghcjs." :
      "" :
      "# In 'production' mode the client code will be compiled while compiling" :
      "# the server (through template haskell). The resulting assets (index.html" :
      "# and javascript files will be embedded into the executable. This allows" :
      "# to distribute the executable as a single file." :
      "production" :
      "" :
      "# In 'development' mode the javascript files will be compiled on the fly" :
      "# from the source files on http requests. Recompilation will be triggered" :
      "# by changes on disks to the source files." :
      "# development" :
      []
    hPutStrLn stderr $ unlines $
      "INFO: The compilation mode of wai-ghcjs is controlled through a file called" :
      (compilationModeFile ++ ". That file didn't exist, so a default file was") :
      "written, including further information." :
      []

data CM a where
  (:>>=) :: CM a -> (a -> CM b) -> CM b

  IO :: IO a -> CM a
  AddDependentFile :: FilePath -> CM ()

instance Functor CM where
  fmap f = \ case
    a :>>= b -> a :>>= (\ x -> f <$> b x)
    IO action -> IO (fmap f action)
    AddDependentFile file ->
      AddDependentFile file :>>= (return . f)

instance Applicative CM where
  pure = IO . pure
  fA <*> xA = do
    f <- fA
    x <- xA
    pure $ f x

instance Monad CM where
  (>>=) = (:>>=)

runCM :: CM a -> Q a
runCM = \ case
  a :>>= b -> runCM a >>= runCM . b
  IO action -> runIO action
  AddDependentFile file -> addDependentFile file
