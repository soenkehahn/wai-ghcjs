
import           Development.Shake
import           Safe
import           System.Directory
import           System.IO
import           System.Process
import           Test.Hspec
import           Test.Mockery.Directory

import           Test.Utils

main :: IO ()
main = do
  repo <- canonicalizePath "."
  project <- canonicalizePath "./test/resources/test-01"
  hspec $ do
    describe "serveGhcjs" $ do
      it "compiles an example project in development mode" $ do
        inTempDirectory $ modifyEnvVar "DEVEL" (const $ Just "1") $ do
          createDirectory "wai-ghcjs-copy"
          unit $ cmd Shell "cp -r" (repo ++ "/*") "wai-ghcjs-copy/"
          unit $ cmd Shell "cp -r" (project ++ "/*") "."
          unit $ cmd (Cwd "client") "stack setup --no-terminal"
          unit $ cmd "stack build --no-terminal"
          withServerExecutable (proc "stack" (words "exec server")) $ \ port -> do
            Stdout index <- cmd "curl" ("localhost:" ++ show port)
            index `shouldContain` "runmain.js"
            Stdout js <- cmd "curl" ("localhost:" ++ show port ++ "/all.js")
            writeFile "test.js" js
            Stdout result <- cmd "node test.js"
            result `shouldBe` "program output"

withServerExecutable :: CreateProcess -> (Int -> IO a) -> IO a
withServerExecutable p action = do
  (Nothing, Just stdout, Nothing, process) <- createProcess p {
    std_out = CreatePipe
  }
  line <- hGetLine stdout
  port <- case readMay line :: Maybe Int of
    Nothing -> error ("unparseable port: " ++ show line)
    Just port -> return port
  r <- action port
  terminateProcess process
  _ <- waitForProcess process
  return r
