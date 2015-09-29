{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Shake.Ghcjs where

import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.Default
import           Data.String.Conversions
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
  let outPattern = buildDir </> takeBaseName mainFile
      outDir = outPattern <.> "jsexe"
      sourceDirFlags = unwords $ map ("-i" ++) sourceDirs
  createDirectoryIfMissing True buildDir
  initializeFiles outPattern
  (exitCode, out, err) <- readProcessWithExitCode "ghcjs"
    ["-O0", mainFile, "-o", outPattern, sourceDirFlags, "-outputdir=" ++ buildDir </> "output"]
    ""
  case pathInfo request of
    [] -> serveFile "text/html" (outDir </> "index" <.> "html")
    [file] | ".js" == takeExtension (cs file) ->
      case exitCode of
        ExitSuccess -> do
          serveFile "application/javascript" (outDir </> cs file)
        ExitFailure _ -> do
          return $ responseLBS ok200 [] (createJsToConsole (out ++ "\n" ++ err))
    _ -> return $ responseLBS notFound404 [] "404 - Not Found"

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

initializeFiles :: String -> IO ()
initializeFiles outPattern = do
  indexExists <- doesFileExist (outPattern <.> "jsexe" </> "index.html")
  when (not indexExists) $ do
    withSystemTempDirectory "serve-ghcjs" $ \ dir -> do
      let mainFile = dir </> "Main.hs"
      writeFile mainFile "main = return ()"
      callCommand (unwords ["ghcjs", "-O0", mainFile, "-o", outPattern])
