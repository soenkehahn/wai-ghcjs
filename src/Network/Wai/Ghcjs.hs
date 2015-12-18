{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Ghcjs (
  mkDevelopmentApp,
  BuildConfig(..),
  Exec(..),

  mkProductionApp,

  serveGhcjs,
 ) where

import           Control.Monad
import           Language.Haskell.TH

import           Network.Wai.Ghcjs.Development
import           Network.Wai.Ghcjs.Internal
import           Network.Wai.Ghcjs.Production

-- * serveGhcjs

-- |
-- 'serveGhcjs' combines 'mkDevelopmentApp' and 'mkProductionApp'. It will
-- read the file @ghcjs-compilation-mode@ with @TemplateHaskell@. If that file
-- contains the word @"development"@ 'mkDevelopmentApp' is used. If it contains
-- @"production"@ 'mkProductionApp' is used.
--
-- The produced splice has the type:
--
-- @'IO' 'Network.Wai.Application'@
--
-- >>> :set -XTemplateHaskell
-- >>> :type $(serveGhcjs (BuildConfig "Main.hs" Nothing [] "test/test-project/client" Vanilla "test-builds"))
-- ...
-- =====> building client code with ghcjs
-- ...
-- =====> done
-- $(serveGhcjs (BuildConfig "Main.hs" Nothing [] "test/test-project/client" Vanilla "test-builds"))
--   :: IO Network.Wai.Application
serveGhcjs :: BuildConfig -> Q Exp
serveGhcjs config = do
  join $ runCM $ ifDevel
    [|mkDevelopmentApp config|]
    (mkProductionApp config)
