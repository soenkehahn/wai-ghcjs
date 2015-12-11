{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Ghcjs.Production where

import           Data.Default ()
import           Data.String.Conversions
import           Development.Shake
import           Language.Haskell.TH
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
-- >>> :type $(mkProductionApp (BuildConfig "Main.hs" Nothing [] "test/resources/test-01/client" Vanilla "test-builds"))
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(mkProductionApp (BuildConfig "Main.hs" Nothing [] "test/resources/test-01/client" Vanilla "test-builds"))
--   :: IO Network.Wai.Application
mkProductionApp :: BuildConfig -> Q Exp
mkProductionApp userConfig = do
  config <- runIO $ prepareConfig "production" userConfig -- fixme: merge runIOs
  outDir <- runIO $ wrapWithMessages $
    runCompiler productionCompiler config
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
-- fixme: add files as dependencies
  let output = dropExtension $ jsExeDir paths
  unit $ cmd
    (addExec (projectExec config) "ghcjs") (mainFile config)
    "-o" output
    (map ("-i" ++) (getSourceDirs config))
    ("-outputdir=" ++ buildDir config </> "output")
  return Success
