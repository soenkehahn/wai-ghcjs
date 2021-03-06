{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Ghcjs.Production where

import           Data.Default ()
import           Data.String.Conversions
import           Development.Shake
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.Wai.Application.Static
import           System.FilePath
import           System.IO
import           WaiAppStatic.Storage.Embedded
import           WaiAppStatic.Types

import           Network.Wai.Ghcjs.Compiler
import           Network.Wai.Ghcjs.Embedded
import           Network.Wai.Ghcjs.Internal

-- | 'mkProductionApp' is similar to 'mkDevelopmentApp' but it is meant for
-- use when you're not developing. It compiles the client application during
-- compilation of the server 'Application' using @TemplateHaskell@. In addition
-- it embeds the compilation results
-- into the executable, so it is completely self-contained. (I.e. you can scp
-- the executable to another server and it will be able to deliver the complete
-- client application.)
--
-- The spliced in fragment has type
--
-- @'IO' 'Network.Wai.Application'@
--
-- >>> :set -XTemplateHaskell
-- >>> :type $(mkProductionApp (BuildConfig "Main.hs" Nothing [] "test/test-project/client" Vanilla "test-builds"))
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(mkProductionApp (BuildConfig "Main.hs" Nothing [] "test/test-project/client" Vanilla "test-builds"))
--   :: IO Network.Wai.Application
mkProductionApp :: BuildConfig -> Q Exp
mkProductionApp userConfig = do
  (outDir, dependentFiles) <- runIO $ wrapWithMessages $ do
    config <- prepareConfig "production" userConfig
    runCompiler productionCompiler config
  mapM_ addDependentFile dependentFiles
  embeddable <- runIO $ mkSettingsFromDir outDir
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

productionCompiler :: Compiler
productionCompiler = Compiler $ \ config paths -> do
  let output = dropExtension $ jsExeDir paths
  unit $ cmd
    (addExec (projectExec config) "ghcjs") (mainFile config)
    "-o" output
    (map ("-i" ++) (getSourceDirs config))
    ("-outputdir=" ++ buildDir config </> "output")
  return Success
