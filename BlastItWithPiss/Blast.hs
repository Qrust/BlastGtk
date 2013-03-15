{-# LANGUAGE StandaloneDeriving #-}
module BlastItWithPiss.Blast
    (module Control.Exception.Lifted

    ,module Network.HTTP.Conduit
    ,module Network.HTTP.Conduit.MultipartFormData
    ,module Network.HTTP.Conduit.Browser

    ,module Network.HTTP.Types

    ,module Network.Mime

    ,userAgents

    ,Blast
    ,generateNewBrowser
    ,runBlastNew
    ,runBlast

    ,BlastProxy(..)
    ,readBlastProxy
    ,maybeNoProxy

    ,httpGetProxy
    ,httpSetProxy
    ,httpWithProxy

    ,httpReq
    ,httpReqLbs
    ,httpReqStr
    ,httpReqStrTags

    ,httpGet
    ,httpGetLbs
    ,httpGetStr
    ,httpGetStrTags
    ) where
import Import
import BlastItWithPiss.MonadChoice

import Control.Exception.Lifted
import Network.Mime
import Network.HTTP.Types
import Network.Socket.Internal
import Network.Socks5
import Network.Socks5.Types
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Network.HTTP.Conduit.MultipartFormData
import qualified Text.Show as Show
import Control.Monad.Trans.Resource

import Text.HTML.TagSoup (Tag)
import Text.HTML.TagSoup.Fast.Utf8Only

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Conduit
import Data.Conduit.List (consume)
import qualified Data.Conduit.List as CL (map)

import Numeric (showHex)

-- HACK HACK HACK unsafePerformIO
import qualified System.IO.Unsafe as Unsafe
{-# NOINLINE userAgent #-}
userAgent :: ByteString
userAgent = Unsafe.unsafePerformIO $ chooseFromList userAgents

-- | Converts a HostAddress to a String in dot-decimal notation
-- Cannibalized from https://github.com/vincenthz/hs-socks/commit/f4a032ce8aaaeed16d051e5ae0f8abadc2f0d0ba
showHostAddress :: HostAddress -> String
showHostAddress num =
    concat [show q1, ".", show q2, ".", show q3, ".", show q4]
  where
    (num',  q1) = num    `quotRem` 256
    (num'', q2) = num'   `quotRem` 256
    (num''',q3) = num''  `quotRem` 256
    (_,     q4) = num''' `quotRem` 256

-- | Converts a IPv6 HostAddress6 to standard hex notation
-- Cannibalized from https://github.com/vincenthz/hs-socks/commit/f4a032ce8aaaeed16d051e5ae0f8abadc2f0d0ba
showHostAddress6 :: HostAddress6 -> String
showHostAddress6 (a,b,c,d) =
    (concat . intersperse ":" . map (flip showHex ""))
        [p1, p2, p3, p4, p5, p6, p7, p8]
  where
    (a',p2) = a  `quotRem` 65536
    (_, p1) = a' `quotRem` 65536
    (b',p4) = b  `quotRem` 65536
    (_, p3) = b' `quotRem` 65536
    (c',p6) = c  `quotRem` 65536
    (_, p5) = c' `quotRem` 65536
    (d',p8) = d  `quotRem` 65536
    (_, p7) = d' `quotRem` 65536

renderSocksHost :: SocksHostAddress -> String
renderSocksHost (SocksAddrDomainName bs) = B8.unpack bs
renderSocksHost (SocksAddrIPV4 ip) = showHostAddress ip
renderSocksHost (SocksAddrIPV6 ip) = showHostAddress6 ip

type Blast = BrowserAction

data BlastProxy = HttpProxy !Proxy
                | SocksProxy !SocksConf
                | NoProxy
    deriving (Eq, Ord)

instance Show BlastProxy where
    show (HttpProxy (Proxy h p)) =
        B8.unpack h ++ ":" ++ show p
    show (SocksProxy s) =
        renderSocksHost (socksHost s) ++ ":" ++ show (socksPort s)
    show NoProxy = "@"

deriving instance Ord SocksVersion

instance Eq SocksConf where
    s1 == s2 =
           socksHost s1 == socksHost s2
        && socksPort s1 == socksPort s2
        && socksVersion s1 == socksVersion s2

instance Ord SocksConf where
    compare s1 s2 =
           compare (renderSocksHost (socksHost s1)) (renderSocksHost (socksHost s2))
        <> compare (socksPort s1) (socksPort s2)
        <> compare (socksVersion s1) (socksVersion s2)

instance NFData Proxy where
    rnf (Proxy h p) = rnf (h,p)

instance NFData PortNumber where
    rnf (PortNum p) = rnf p

instance NFData SocksHostAddress where
    rnf (SocksAddrDomainName bs) = rnf bs
    rnf (SocksAddrIPV4 ip) = rnf ip
    rnf (SocksAddrIPV6 ip) = rnf ip

instance NFData SocksVersion where

instance NFData SocksConf where
    rnf s =       socksHost s
        `deepseq` socksPort s
        `deepseq` socksVersion s
        `deepseq` ()

instance NFData BlastProxy where
    rnf (HttpProxy p) = rnf p
    rnf (SocksProxy p) = rnf p
    rnf NoProxy = ()

readBlastProxy :: Bool -> String -> Maybe BlastProxy
readBlastProxy isSocks s =
    case break (==':') (dropWhile isSpace s) of
        (host@(_:_), (_:port)) ->
            if isSocks
                then SocksProxy . defaultSocksConf host . (fromIntegral :: Int -> PortNumber) <$> readMay port
                else HttpProxy . Proxy (fromString host) <$> readMay port
        _ -> Nothing

maybeNoProxy :: a -> (BlastProxy -> a) -> BlastProxy -> a
maybeNoProxy v _ NoProxy = v
maybeNoProxy _ f p = f p

userAgents :: [ByteString]
userAgents =
    [
-- Windows
     "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US) AppleWebKit/527  (KHTML, like Gecko, Safari/419.3) Arora/0.6 (Change: )"
    ,"Mozilla/5.0 (Windows; U; ; en-NZ) AppleWebKit/527  (KHTML, like Gecko, Safari/419.3) Arora/0.8.0"
    ,"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Avant Browser; Avant Browser; .NET CLR 1.0.3705; .NET CLR 1.1.4322; Media Center PC 4.0; .NET CLR 2.0.50727; .NET CLR 3.0.04506.30)"
    ,"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.8 (KHTML, like Gecko) Beamrise/17.2.0.9 Chrome/17.0.939.0 Safari/535.8"
    ,"Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/18.6.872.0 Safari/535.2 UNTRUSTED/1.0 3gpp-gba UNTRUSTED/1.0"
    ,"Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3"
    ,"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1092.0 Safari/536.6"
    ,"Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6"
    ,"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1"
    ,"Mozilla/5.0 (Windows NT 6.1; WOW64; rv:10.0.1) Gecko/20100101 Firefox/10.0.1"
    ,"Mozilla/5.0 (Windows NT 6.1; rv:12.0) Gecko/20120403211507 Firefox/12.0"
    ,"Mozilla/5.0 (Windows NT 6.0; rv:14.0) Gecko/20100101 Firefox/14.0.1"
    ,"Mozilla/5.0 (Windows NT 6.1; WOW64; rv:15.0) Gecko/20120427 Firefox/15.0a1"
    ,"Mozilla/5.0 (Windows NT 6.2; Win64; x64; rv:16.0) Gecko/16.0 Firefox/16.0"
    ,"Mozilla/5.0 (compatible; Konqueror/4.5; Windows) KHTML/4.5.4 (like Gecko)"
    ,"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US) AppleWebKit/533.1 (KHTML, like Gecko) Maxthon/3.0.8.2 Safari/533.1"
    ,"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)"
    ,"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)"
    ,"Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0)"
    ,"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Trident/4.0)"
    ,"Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0)"
    ,"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Trident/5.0)"
    ,"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"
    ,"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.2; Trident/5.0)"
    ,"Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.2; WOW64; Trident/5.0)"
    ,"Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)"
    ,"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Trident/6.0)"
    ,"Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0"
    ,"Opera/9.25 (Windows NT 6.0; U; en)"
    ,"Opera/9.80 (Windows NT 5.2; U; en) Presto/2.2.15 Version/10.10"
    ,"Opera/9.80 (Windows NT 5.1; U; ru) Presto/2.7.39 Version/11.00"
    ,"Opera/9.80 (Windows NT 6.1; U; en) Presto/2.7.62 Version/11.01"
    ,"Opera/9.80 (Windows NT 5.1; U; zh-tw) Presto/2.8.131 Version/11.10"
    ,"Opera/9.80 (Windows NT 6.1; U; es-ES) Presto/2.9.181 Version/12.00"
    ,"Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/531.21.8 (KHTML, like Gecko) Version/4.0.4 Safari/531.21.10"
    ,"Mozilla/5.0 (Windows; U; Windows NT 5.2; en-US) AppleWebKit/533.17.8 (KHTML, like Gecko) Version/5.0.1 Safari/533.17.8"
    ,"Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/533.19.4 (KHTML, like Gecko) Version/5.0.2 Safari/533.18.5"
    ,"Mozilla/5.0 (Windows; U; Windows NT 6.1; en-GB; rv:1.9.1.17) Gecko/20110123 (like Firefox/3.x) SeaMonkey/2.0.12"
    ,"Mozilla/5.0 (Windows NT 5.2; rv:10.0.1) Gecko/20100101 Firefox/10.0.1 SeaMonkey/2.7.1"
-- Mac
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:2.0.1) Gecko/20100101 Firefox/4.0.1 Camino/2.2.1"
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:2.0b6pre) Gecko/20100907 Firefox/4.0b6pre Camino/2.2a1pre"
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_0) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3"
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.4 (KHTML like Gecko) Chrome/22.0.1229.79 Safari/537.4"
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_2; rv:10.0.1) Gecko/20100101 Firefox/10.0.1"
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:16.0) Gecko/20120813 Firefox/16.0"
    ,"iTunes/4.2 (Macintosh; U; PPC Mac OS X 10.2)"
    ,"iTunes/9.0.3 (Macintosh; U; Intel Mac OS X 10_6_2; en-ca)"
    ,"Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US) AppleWebKit/528.16 (KHTML, like Gecko, Safari/528.16) OmniWeb/v622.8.0.112941"
    ,"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_6; en-US) AppleWebKit/528.16 (KHTML, like Gecko, Safari/528.16) OmniWeb/v622.8.0"
    ,"Opera/9.20 (Macintosh; Intel Mac OS X; U; en)"
    ,"Opera/9.64 (Macintosh; PPC Mac OS X; U; en) Presto/2.1.1"
    ,"Opera/9.80 (Macintosh; Intel Mac OS X; U; en) Presto/2.6.30 Version/10.61"
    ,"Opera/9.80 (Macintosh; Intel Mac OS X 10.4.11; U; en) Presto/2.7.62 Version/11.00"
    ,"Opera/9.80 (Macintosh; Intel Mac OS X 10.6.8; U; fr) Presto/2.9.168 Version/11.52"
    ,"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_2; en-us) AppleWebKit/531.21.8 (KHTML, like Gecko) Version/4.0.4 Safari/531.21.10"
    ,"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_5; de-de) AppleWebKit/534.15  (KHTML, like Gecko) Version/5.0.3 Safari/533.19.4"
    ,"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-us) AppleWebKit/533.20.25 (KHTML, like Gecko) Version/5.0.4 Safari/533.20.27"
    ,"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_7; en-us) AppleWebKit/534.20.8 (KHTML, like Gecko) Version/5.1 Safari/534.20.8"
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_3) AppleWebKit/534.55.3 (KHTML, like Gecko) Version/5.1.3 Safari/534.53.10"
    ,"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.5; rv:10.0.1) Gecko/20100101 Firefox/10.0.1 SeaMonkey/2.7.1"
-- Linux
    ,"Mozilla/5.0 (X11; U; Linux; en-US) AppleWebKit/527  (KHTML, like Gecko, Safari/419.3) Arora/0.10.1"
    ,"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.9 Safari/536.5"
    ,"Mozilla/5.0 (X11; CrOS i686 2268.111.0) AppleWebKit/536.11 (KHTML, like Gecko) Chrome/20.0.1132.57 Safari/536.11"
    ,"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.4 (KHTML like Gecko) Chrome/22.0.1229.56 Safari/537.4"
    ,"Mozilla/4.0 (compatible; Dillo 3.0)"
    ,"Mozilla/5.0 (X11; U; Linux i686; en-us) AppleWebKit/528.5  (KHTML, like Gecko, Safari/528.5 ) lt-GtkLauncher"
    ,"Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.1.16) Gecko/20120421 Gecko Firefox/11.0"
    ,"Mozilla/5.0 (X11; Linux i686; rv:12.0) Gecko/20100101 Firefox/12.0 "
    ,"Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:14.0) Gecko/20100101 Firefox/14.0.1"
    ,"Mozilla/5.0 (X11; Linux i686; rv:19.0) Gecko/20100101 Firefox/19.0"
    ,"Mozilla/5.0 (X11; Linux i686; rv:16.0) Gecko/20100101 Firefox/16.0"
    ,"Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.8) Gecko Galeon/2.0.6 (Ubuntu 2.0.6-2)"
    ,"Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.16) Gecko/20080716 (Gentoo) Galeon/2.0.6"
    ,"Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.1.13) Gecko/20100916 Iceape/2.0.8"
    ,"Mozilla/5.0 (X11; Linux i686; rv:14.0) Gecko/20100101 Firefox/14.0.1 Iceweasel/14.0.1"
    ,"Mozilla/5.0 (X11; Linux x86_64; rv:15.0) Gecko/20120724 Debian Iceweasel/15.02"
    ,"Mozilla/5.0 (compatible; Konqueror/4.2; Linux) KHTML/4.2.4 (like Gecko) Slackware/13.0"
    ,"Mozilla/5.0 (compatible; Konqueror/4.3; Linux) KHTML/4.3.1 (like Gecko) Fedora/4.3.1-3.fc11"
    ,"Mozilla/5.0 (compatible; Konqueror/4.4; Linux) KHTML/4.4.1 (like Gecko) Fedora/4.4.1-1.fc12"
    ,"Mozilla/5.0 (compatible; Konqueror/4.4; Linux 2.6.32-22-generic; X11; en_US) KHTML/4.4.3 (like Gecko) Kubuntu"
    ,"Midori/0.1.10 (X11; Linux i686; U; en-us) WebKit/(531).(2) "
    ,"Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.0.3) Gecko/2008092814 (Debian-3.0.1-1)"
    ,"Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9a3pre) Gecko/20070330"
    ,"Opera/9.64 (X11; Linux i686; U; Linux Mint; nb) Presto/2.1.1"
    ,"Opera/9.80 (X11; Linux i686; U; en) Presto/2.2.15 Version/10.10"
    ,"Opera/9.80 (X11; Linux x86_64; U; pl) Presto/2.7.62 Version/11.00"
    ,"Mozilla/5.0 (X11; Linux i686) AppleWebKit/534.34 (KHTML, like Gecko) QupZilla/1.2.0 Safari/534.34"
    ,"Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.1.17) Gecko/20110123 SeaMonkey/2.0.12"
    ,"Mozilla/5.0 (X11; Linux i686; rv:10.0.1) Gecko/20100101 Firefox/10.0.1 SeaMonkey/2.7.1"
    ,"Mozilla/5.0 (X11; U; Linux x86_64; us; rv:1.9.1.19) Gecko/20110430 shadowfox/7.0 (like Firefox/7.0"
    ,"Mozilla/5.0 (X11; U; Linux i686; it; rv:1.9.2.3) Gecko/20100406 Firefox/3.6.3 (Swiftfox)"
    ,"Uzbl (Webkit 1.3) (Linux i686 [i686])"
-- *nix
    ,"Mozilla/5.0 (Unknown; U; UNIX BSD/SYSV system; C -) AppleWebKit/527  (KHTML, like Gecko, Safari/419.3) Arora/0.10.2"
    ,"Mozilla/5.0 (X11; FreeBSD amd64) AppleWebKit/536.5 (KHTML like Gecko) Chrome/19.0.1084.56 Safari/536.5"
    ,"Mozilla/5.0 (X11; FreeBSD amd64) AppleWebKit/537.4 (KHTML like Gecko) Chrome/22.0.1229.79 Safari/537.4"
    ,"Mozilla/5.0 (X11; U; OpenBSD arm; en-us) AppleWebKit/531.2  (KHTML, like Gecko) Safari/531.2  Epiphany/2.30.0"
    ,"Mozilla/5.0 (X11; U; FreeBSD amd64; en-us) AppleWebKit/531.2  (KHTML, like Gecko) Safari/531.2  Epiphany/2.30.0"
    ,"Mozilla/5.0 (X11; U; SunOS i86pc; en-US; rv:1.9.1b3) Gecko/20090429 Firefox/3.1b3"
    ,"Mozilla/5.0 (X11; U; OpenBSD i386; en-US; rv:1.9.1) Gecko/20090702 Firefox/3.5"
    ,"Mozilla/5.0 (X11; U; FreeBSD i386; de-CH; rv:1.9.2.8) Gecko/20100729 Firefox/3.6.8"
    ,"Mozilla/5.0 (X11; FreeBSD amd64; rv:5.0) Gecko/20100101 Firefox/5.0"
    ,"Mozilla/5.0 (compatible; Konqueror/4.1; DragonFly) KHTML/4.1.4 (like Gecko)"
    ,"Mozilla/5.0 (compatible; Konqueror/4.1; OpenBSD) KHTML/4.1.4 (like Gecko)"
    ,"Mozilla/5.0 (compatible; Konqueror/4.5; NetBSD 5.0.2; X11; amd64; en_US) KHTML/4.5.4 (like Gecko)"
    ,"Mozilla/5.0 (compatible; Konqueror/4.5; FreeBSD) KHTML/4.5.4 (like Gecko)"
    ,"Mozilla/5.0 (X11; U; NetBSD amd64; en-US; rv:1.9.2.15) Gecko/20110308 Namoroka/3.6.15"
    ,"NetSurf/1.2 (NetBSD; amd64)"
    ,"Opera/9.80 (X11; FreeBSD 8.1-RELEASE i386; Edition Next) Presto/2.12.388 Version/12.10"
    ,"Mozilla/5.0 (X11; U; SunOS i86pc; en-US; rv:1.8.1.12) Gecko/20080303 SeaMonkey/1.1.8"
    ]

generateNewBrowser :: BrowserAction ()
generateNewBrowser = do
    setMaxRedirects Nothing
    setMaxRetryCount 1
    setTimeout $ Just $ 10 * 1000000
    setDefaultHeader hUserAgent $ Just userAgent
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
