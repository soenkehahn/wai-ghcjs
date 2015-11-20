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

data BuildConfig = BuildConfig {
  mainFile :: FilePath
, sourceDirs :: [FilePath]
, projectDir :: FilePath
, projectExec :: Exec
, buildDir :: FilePath
} deriving (Eq, Show)

instance Lift BuildConfig where
  lift = \ case
    BuildConfig a b c d e -> [|BuildConfig a b c d e|]

getSourceDirs :: BuildConfig -> [FilePath]
getSourceDirs config = case sourceDirs config of
  [] -> ["."]
  dirs -> dirs

data Exec
  = Vanilla
  | Cabal
  | Stack
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
