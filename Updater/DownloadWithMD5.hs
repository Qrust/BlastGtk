module Updater.DownloadWithMD5 where
import Import
import Updater.Manifest(URL, MD5Sum, renderMD5)
import Network.HTTP.Conduit
import Crypto.Classes

-- | Download file from URL and compare it's md5sum with supplied.
-- returns Just file if sums match.
downloadWithMD5 :: URL -> MD5Sum -> IO (Maybe LByteString)
downloadWithMD5 url md5 = do
    lbs <- responseBody <$> withManager (httpLbs $ fromJust $ parseUrl url)
    if renderMD5 (hash lbs) == md5
        then return $ Just lbs
        else return Nothing
