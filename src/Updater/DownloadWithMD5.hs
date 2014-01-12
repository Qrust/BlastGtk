module Updater.DownloadWithMD5 where
import Import
import Updater.Manifest(MD5Sum, renderMD5)
import Network.HTTP.Conduit
import Crypto.Classes

-- | Download file from URL and compare it's md5sum with supplied.
-- returns Just file if sums match.
downloadWithMD5 :: String -> MD5Sum -> IO (Maybe LByteString)
downloadWithMD5 url md5 = do
    u <- parseUrl url
    lbs <- responseBody <$> withManager (httpLbs u)
    if renderMD5 (hash lbs) == md5
        then return $ Just lbs
        else return Nothing
