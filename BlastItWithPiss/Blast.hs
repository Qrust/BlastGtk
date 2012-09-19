module BlastItWithPiss.Blast
    (module Control.Exception.Lifted
    ,module Network.HTTP.Conduit
    ,module Network.HTTP.Conduit.Browser
    ,module Network.Mime
    ,module Network.HTTP.Types
    ,userAgent
    ,Blast
    ,canonicalizeBrowser
    ,canonicalizeReq
    ,runBlast
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
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import qualified Codec.Binary.UTF8.Generic as UTF8
import Control.Monad.Trans.Resource

userAgent :: ByteString
userAgent = "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0)"

type Blast = BrowserAction

canonicalizeBrowser :: BrowserAction ()
canonicalizeBrowser = do
    setUserAgent $ Just userAgent
    setMaxRedirects Nothing
    setTimeout $ Just $ 10 * 1000000
    --FIXME
    setCookieFilter $ \_ _ -> return False
    --

canonicalizeReq :: Request a -> Request a
canonicalizeReq = id

runBlast :: Blast a -> IO a
runBlast f = 
    withManager (`browse` do
        canonicalizeBrowser
        f)

httpReq :: Request (ResourceT IO) -> Blast (Response LByteString)
httpReq = makeRequestLbs

httpReqStr :: Request (ResourceT IO) -> Blast String
httpReqStr u = UTF8.toString . responseBody <$> httpReq u

httpReqStrTags :: Request (ResourceT IO) -> Blast [Tag String]
httpReqStrTags u = parseTags <$> httpReqStr u

httpGet :: String -> Blast LByteString
httpGet u = do
    r <- canonicalizeReq <$> parseUrl u
    responseBody <$> httpReq r

httpGetStr :: String -> Blast String
httpGetStr u = UTF8.toString <$> httpGet u

httpGetStrTags :: String -> Blast [Tag String]
httpGetStrTags u = parseTags <$> httpGetStr u
