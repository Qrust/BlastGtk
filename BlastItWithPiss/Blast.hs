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
    ,userAgents
    ,generateNewBrowser
    ,runBlastNew
    ,runBlast
    ,httpSetProxy
    ,httpReq
    ,httpReqLbs
    ,httpReqStr
    ,httpReqStrTags
    ,httpGet
    ,httpGetLbs
    ,httpGetStr
    ,httpGetStrTags
    ,httpGetProxy
    ,httpWithProxy
    ) where
import Import
import BlastItWithPiss.MonadChoice
import Control.Exception.Lifted
import Network.Mime
import Network.HTTP.Types
import Network.Socket.Internal
import Network.Socks5
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import qualified Text.Show as Show
import Control.Monad.Trans.Resource

import Text.HTML.TagSoup (Tag)
import Text.HTML.TagSoup.Fast

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Conduit
import Data.Conduit.List as CL

-- HACK unsafePerformIO
import qualified System.IO.Unsafe as Unsafe

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

userAgents :: [ByteString]
userAgents =
    ["Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0)"
    ,"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"
    ,"Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0"
    ,"Opera/9.80 (Windows NT 6.1; U; es-ES) Presto/2.9.181 Version/12.00"
    ,"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1"
    ,"Mozilla/5.0 (Windows NT 6.2; Win64; x64; rv:16.0) Gecko/16.0 Firefox/16.0"
    ]

-- HACK HACK HACK unsafePerformIO
userAgent :: ByteString
userAgent = Unsafe.unsafePerformIO $ chooseFromList userAgents

generateNewBrowser :: BrowserAction ()
generateNewBrowser = do
    setMaxRedirects Nothing
    setMaxRetryCount 1
    setTimeout $ Just $ 10 * 1000000
    setUserAgent $ Just userAgent
    setOverrideHeaders [(hAcceptLanguage, "ru;q=1.0, en;q=0.1")
                       ,(hConnection, "keep-alive")]
    --
    --setCookieFilter $ \_ _ -> return False
    --

runBlastNew :: Manager -> Blast a -> IO a
runBlastNew m blast = 
    runResourceT $ browse m $ do
        generateNewBrowser
        blast

runBlast :: Manager -> BrowserState -> Blast a -> IO a
runBlast m st blast =
    runResourceT $ browse m $ do
        setBrowserState st
        blast

httpSetProxys :: Maybe Proxy -> Maybe SocksConf -> Blast ()
httpSetProxys h s = do
    setCurrentProxy h
    setCurrentSocksProxy s

httpSetProxy :: BlastProxy -> Blast ()
httpSetProxy NoProxy = httpSetProxys Nothing Nothing
httpSetProxy (HttpProxy p) = httpSetProxys (Just p) Nothing
httpSetProxy (SocksProxy p) = httpSetProxys Nothing (Just p)

httpGetProxy :: Blast BlastProxy
httpGetProxy = do
    h <- getCurrentProxy
    case h of
        Just p -> return $ HttpProxy p
        Nothing -> do
            s <- getCurrentSocksProxy
            case s of
                Just p -> return $ SocksProxy p
                Nothing -> return NoProxy

httpWithProxy :: BlastProxy -> Blast a -> Blast a
httpWithProxy p m = do
    bracket
        httpGetProxy
        (\current -> httpSetProxy current)
        (\_ -> do
            httpSetProxy p
            m)

httpReq :: Request (ResourceT IO) -> Blast (Response (ResumableSource (ResourceT IO) ByteString))
httpReq = makeRequest

httpReqLbs :: Request (ResourceT IO) -> Blast (Response LByteString)
httpReqLbs = makeRequestLbs

httpReqStr :: Request (ResourceT IO) -> Blast (Response Text)
httpReqStr u = do
    x <- httpReq u
    liftIO $ runResourceT $ (<$ x) . T.concat <$> (responseBody x $$+- CL.map T.decodeUtf8 =$ consume)

httpReqStrTags :: Request (ResourceT IO) -> Blast (Response [Tag Text])
httpReqStrTags u = do
    x <- httpReq u
    liftIO $ runResourceT $ (<$ x) . parseTagsT . S.concat <$> (responseBody x $$+- consume)

httpGet :: String -> Blast (Response (ResumableSource (ResourceT IO) ByteString))
httpGet = makeRequest . fromJust . parseUrl

httpGetLbs :: String -> Blast LByteString
httpGetLbs u = do
    r <- parseUrl u
    responseBody <$> makeRequestLbs r

httpGetStr :: String -> Blast Text
httpGetStr u = do
    g <- httpGet u
    let x = liftIO $ runResourceT $ T.concat <$> (responseBody g $$+- CL.map T.decodeUtf8 =$ consume)
    x

httpGetStrTags :: String -> Blast [Tag Text]
httpGetStrTags u = do
    g <- httpGet u
    let x = liftIO $ runResourceT $ parseTagsT . S.concat <$> (responseBody g $$+- consume)
    x
