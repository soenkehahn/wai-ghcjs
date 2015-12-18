
import           Control.Concurrent
import qualified Data.ByteString.Lazy as BS
import           Data.String.Conversions
import           Development.Shake
import           Safe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Test.Hspec
import           Test.Mockery.Directory

import           Test.Utils

main :: IO ()
main = do
  hspec $ before getRepo $ do
    describe "serveGhcjs" $ do
      it "compiles an example project in development mode" $ \ repo -> do
        withCompilationModeFile "development" $ integrationTest repo

      it "compiles an example project in production mode" $ \ repo -> do
        withCompilationModeFile "production" $ integrationTest repo

getRepo :: IO FilePath
getRepo = canonicalizePath "."

integrationTest :: FilePath -> IO ()
integrationTest repo = do
  let project = repo </> "test/test-project"
  createDirectory "wai-ghcjs-copy"
  unit $ cmd Shell "cp -r" (repo ++ "/*") "wai-ghcjs-copy/"
  unit $ cmd Shell "cp -r" (project ++ "/*") "."
  unit $ cmd (Cwd "client") "stack setup --no-terminal"
  unit $ cmd "stack build --no-terminal"
  withServerExecutable (proc "stack" (words "exec server")) $ \ port -> do
    index <- curl ("localhost:" ++ show port)
    index `shouldContain` "runmain.js"
    js <- curl ("localhost:" ++ show port ++ "/all.js")
    writeFile "test.js" js
    Stdout result <- cmd "node test.js"
    result `shouldBe` "program output"

curl :: String -> IO String
curl url = do
  (Nothing, Just stdout, Nothing, process) <- createProcess (proc "curl" ["-s", url]) {
    std_out = CreatePipe
  }
  output <- BS.hGetContents stdout
  return $ cs $ decompress output

withServerExecutable :: CreateProcess -> (Int -> IO a) -> IO a
withServerExecutable p action = do
  (Nothing, Just stdout, Nothing, process) <- createProcess p {
    std_out = CreatePipe
  }
  line <- hGetLine stdout
  port <- case readMay line :: Maybe Int of
    Nothing -> error ("unparseable port: " ++ show line)
    Just port -> return port
  _ <- forkIO $ (hGetContents stdout >>= putStr)
  r <- action port
  terminateProcess process
  _ <- waitForProcess process
  return r
