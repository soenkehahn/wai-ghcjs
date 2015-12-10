{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Shake.Ghcjs (
  mkDevelopmentApp,
  BuildConfig(..),
  Exec(..),

  mkProductionApp,

  serveGhcjs,
  -- Environment(..),
 ) where

import           Language.Haskell.TH
import           Network.Wai
import           System.Environment

import           Network.Wai.Shake.Ghcjs.Development
import           Network.Wai.Shake.Ghcjs.Internal
import           Network.Wai.Shake.Ghcjs.Production

-- * serveGhcjs

-- | fixme: docs
--
-- 'serveGhcjs' combines 'mkDevelopmentApp' and 'mkProductionApp'. It will
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
-- >>> :set -XTemplateHaskell
-- >>> :type $(serveGhcjs (BuildConfig "Main.hs" Nothing [] "test/resources/test-01" Vanilla "test-builds"))
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(serveGhcjs (BuildConfig "Main.hs" Nothing [] "test/resources/test-01" Vanilla "test-builds"))
--   :: IO Application
--
-- So the 'BuildConfig' argument has to be supplied inside the @TemplateHaskell@
-- brackets while the 'Environment' argument has to be outside:
--
-- >>> :type $(serveGhcjs (BuildConfig "Main.hs" Nothing [] "test/resources/test-01" Vanilla "test-builds"))
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(serveGhcjs (BuildConfig "Main.hs" Nothing [] "test/resources/test-01" Vanilla "test-builds"))
--   :: IO Application
--
-- This way you can decide at runtime (e.g. depending on a command line flag)
-- whether to run in 'Development' or 'Production' mode.
serveGhcjs :: BuildConfig -> Q Exp
serveGhcjs config = do
  devel <- runIO $ lookupEnv "DEVEL"
  case devel of
    Just _ -> [|mkDevelopmentApp config|]
    Nothing -> mkProductionApp config
