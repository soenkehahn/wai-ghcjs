
import           Control.Monad
import           Development.Shake
import           System.Directory
import           System.Exit
import           System.FilePath
import           Test.DocTest

import           Network.Wai.Ghcjs.Internal
import           Test.Utils

main :: IO ()
main = do
  unit $ cmd (Cwd "test/test-project/client") "stack setup --no-terminal"
  Stdout ghcjs <- cmd (Cwd "test/test-project/client") "stack exec -- which ghcjs"
  modifyEnvVar "PATH" (fmap (\ path -> takeDirectory ghcjs ++ ":" ++ path)) $ do
    exists <- System.Directory.doesFileExist compilationModeFile
    when exists $
      die ("please delete " ++ compilationModeFile)
    doctest ["src"]
    removeFile compilationModeFile
