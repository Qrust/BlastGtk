module BlastItWithPiss.Blast
    (module Control.Exception.Lifted
    ,module Network.HTTP.Conduit
    ,module Network.HTTP.Conduit.Browser
    ,module Network.Mime
    ,module Network.HTTP.Types
    ,Blast
    ,BlastProxy(..)
    ,readBlastProxy
    ,maybeNoProxy
    ,userAgent
    ,canonicalizeBrowser
    ,runBlast
    ,httpSetProxy
    ,httpReq
    ,httpReqStr
    ,httpReqStrTags
    ,httpGet
    ,httpGetStr
    ,httpGetStrTags
    ) where
import Import
import Control.Exception.Lifted
import Text.HTML.TagSoup
import Network.Mime
import Network.HTTP.Types
import Network.Socket.Internal
import Network.Socks5
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import qualified Text.Show as Show
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as B8
import BlastItWithPiss.Parsing()
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T


type Blast = BrowserAction

data BlastProxy = HttpProxy !Proxy
                | SocksProxy !SocksConf
                | NoProxy
    deriving (Eq, Ord)

instance Show BlastProxy where
    show (HttpProxy (Proxy h p)) =
        B8.unpack h ++ ":" ++ show p
    show (SocksProxy (SocksConf h (PortNum p) _)) =
        h ++ ":" ++ show p
    show NoProxy = "@"

instance Eq Proxy where
    (Proxy h1 p1) == (Proxy h2 p2) = h1 == h2 && p1 == p2

instance Eq SocksConf where
    (SocksConf h1 p1 v1) == (SocksConf h2 p2 v2) =
        h1==h2 && p1 == p2 && v1 == v2

instance Ord Proxy where
    compare (Proxy h1 p1) (Proxy h2 p2) =
        compare h1 h2 <> compare p1 p2

instance Ord SocksConf where
    compare (SocksConf h1 p1 v1) (SocksConf h2 p2 v2) =
        compare h1 h2 <> compare p1 p2 <> compare v1 v2

instance NFData Proxy where
    rnf (Proxy h p) = rnf (h,p)

instance NFData PortNumber where
    rnf (PortNum p) = rnf p

instance NFData SocksConf where
    rnf (SocksConf h p v) = rnf (h,p,v)

instance NFData BlastProxy where
    rnf (HttpProxy p) = rnf p
    rnf (SocksProxy p) = rnf p
    rnf NoProxy = ()

readBlastProxy :: Bool -> String -> Maybe BlastProxy
readBlastProxy isSocks s =
    case break (==':') (dropWhile isSpace s) of
        (host@(_:_), (_:port)) ->
            if isSocks
                then SocksProxy . defaultSocksConf host . PortNum <$> readMay port
                else HttpProxy . Proxy (fromString host) <$> readMay port
        _ -> Nothing

maybeNoProxy :: a -> (BlastProxy -> a) -> BlastProxy -> a
maybeNoProxy v _ NoProxy = v
maybeNoProxy _ f p = f p

userAgent :: ByteString
userAgent = "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0)"

canonicalizeBrowser :: BrowserAction ()
canonicalizeBrowser = do
    setUserAgent $ Just userAgent
    setMaxRedirects Nothing
    setTimeout $ Just $ 10 * 1000000
    --
    --setCookieFilter $ \_ _ -> return False
    --

runBlast :: Blast a -> IO a
runBlast f = 
    withManager (`browse` do
        canonicalizeBrowser
        f)

httpSetProxys :: Maybe Proxy -> Maybe SocksConf -> Blast ()
httpSetProxys h s = do
    setCurrentProxy h
    setCurrentSocksProxy s

httpSetProxy :: BlastProxy -> Blast ()
httpSetProxy NoProxy = httpSetProxys Nothing Nothing
httpSetProxy (HttpProxy p) = httpSetProxys (Just p) Nothing
httpSetProxy (SocksProxy p) = httpSetProxys Nothing (Just p)

httpReq :: Request (ResourceT IO) -> Blast (Response LByteString)
httpReq = makeRequestLbs

httpReqStr :: Request (ResourceT IO) -> Blast (Response T.Text)
httpReqStr u = fmap T.decodeUtf8 <$> httpReq u

httpReqStrTags :: Request (ResourceT IO) -> Blast (Response [Tag T.Text])
httpReqStrTags u = fmap (parseTags . T.decodeUtf8) <$> httpReq u

httpGet :: String -> Blast LByteString
httpGet u = do
    r <- parseUrl u
    responseBody <$> makeRequestLbs r

httpGetStr :: String -> Blast T.Text
httpGetStr u = do
    g <- httpGet u
    let x = g `deepseq` T.decodeUtf8 g
    x `deepseq` return x

httpGetStrTags :: String -> Blast [Tag T.Text]
httpGetStrTags u = do
    g <- httpGetStr u
    let x = g `deepseq` parseTags g
    x `deepseq` return x
