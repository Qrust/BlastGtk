module BlastItWithPiss.ProxyReader
    (filenameIsSocks
    ,parseProxyFile
    ,readProxyFile) where
import Import

import BlastItWithPiss.Blast

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

filenameIsSocks :: String -> Bool
filenameIsSocks fname = "socks" `isInfixOf` map toLower fname

parseProxyFile :: Bool -> Text -> [Either Text BlastProxy]
parseProxyFile socks t =
    map (\ip ->
      case readBlastProxy socks (takeWhile (\c -> c /= '|' && c /= '#') $ T.unpack ip) of
        Nothing -> Left ip
        Just p  -> Right p)
    $ filter (\x -> not (T.null x) && not (T.all isSpace x))
    $ T.lines t

readProxyFile
    :: Bool -> FilePath -> IO [Either Text BlastProxy]
readProxyFile socks f = do
    fromIOException (return []) $ do
        withFile f ReadMode $ \h -> do
            hSetEncoding h utf8
            hSetNewlineMode h universalNewlineMode
            parseProxyFile socks <$> TIO.hGetContents h
