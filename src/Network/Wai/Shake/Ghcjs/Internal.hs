{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Shake.Ghcjs.Internal where

import qualified Data.ByteString.Lazy as LBS
import           Data.Default ()
import           Data.String.Conversions
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.CodeGen
import           Language.Haskell.TH.Lift
import           System.Directory as System
import           System.Exit
import           System.FilePath

-- | Specifies how to build the client application.
data BuildConfig = BuildConfig {
  mainFile :: FilePath
    -- ^ location of the main module
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
    BuildConfig a b c d e -> [|BuildConfig a b c d e|]

getSourceDirs :: BuildConfig -> [FilePath]
getSourceDirs config = case sourceDirs config of
  [] -> ["."]
  dirs -> dirs

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

data Environment
  = Development
  | Production
  deriving (Eq, Show)

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

createJsToConsole :: String -> LBS.ByteString
createJsToConsole msg =
  let escape :: String -> String
      escape s =
        show $ prettyPrint (string (doublePercentSigns s) :: Expression ())
      doublePercentSigns = concatMap (\ c -> if c == '%' then "%%" else [c])
  in cs $ unlines $ map (\ line -> "console.log(" ++ escape line ++ ");") (lines msg)
