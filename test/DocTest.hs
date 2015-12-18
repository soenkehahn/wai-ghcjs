
import           Development.Shake
import           System.FilePath
import           Test.DocTest

import           Test.Utils

main :: IO ()
main = do
  unit $ cmd (Cwd "test/test-project/client") "stack setup --no-terminal"
  Stdout ghcjs <- cmd (Cwd "test/test-project/client") "stack exec -- which ghcjs"
  modifyEnvVar "PATH" (fmap (\ path -> takeDirectory ghcjs ++ ":" ++ path)) $ do
    doctest ["src"]
