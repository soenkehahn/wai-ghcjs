
module Network.Wai.Ghcjs.Embedded (mkSettingsFromDir) where

import           Crypto.Hash.MD5 (hashlazy)
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Lazy (ByteString, readFile)
import           Data.List
import           Data.String.Conversions
import           Data.Text (Text, take)
import           Data.Text.Encoding
import           System.Directory
import           System.FilePath
import           WaiAppStatic.Storage.Embedded

mkSettingsFromDir :: FilePath -> IO [EmbeddableEntry]
mkSettingsFromDir dir = do
  files <-
    filter (\ f -> takeExtension f `elem` [".html", ".js"]) <$>
    sort <$>
    getDirectoryContents dir
  mapM (mkEmbeddable dir) files

mkEmbeddable :: FilePath -> FilePath -> IO EmbeddableEntry
mkEmbeddable dir file = do
  contents <- Data.ByteString.Lazy.readFile (dir </> file)
  let mimetype = cs $ case takeExtension file of
        ".html" -> "text/html"
        ".js" -> "application/javascript"
        x -> error ("unknown extension: " ++ x)
  return $ EmbeddableEntry {
    eLocation = cs file,
    eMimeType = mimetype,
    eContent  = Left (hash contents, contents)
  }

hash :: ByteString -> Text
hash = Data.Text.take 8 . decodeUtf8 . B64.encode . hashlazy
