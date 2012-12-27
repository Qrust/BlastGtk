module BlastItWithPiss.Blast
    (
    --FIXME
    genOriginStamp,
    --FIXME

     module Control.Exception.Lifted
    ,module Network.Mime
    ,module Network.HTTP.Conduit
    ,module Network.HTTP.Conduit.Browser
    ,module Network.HTTP.Types
    ,module BlastItWithPiss.Proxy
    ,module BlastItWithPiss.Origin
    
    ,Blast
    ,PerWipe(..)
    ,PerBoard(..)
    ,PerProxy(..)
    ,mkEmptyPerProxy

    ,LogDetail(..)
    ,BlastData(..)

    ,CaptchaType(..)
    ,LogType(..)

    ,CaptchaAnswer(..)
    ,Message(..)
    ,OutMessage(..)

    ,runBlast
    ,recMode
    ,recThread
    ,log
    ,debug
    ,sendOut

    ,setupBlast
    ,runHttp
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
import BlastItWithPiss.Proxy
import BlastItWithPiss.UserAgent
import BlastItWithPiss.Origin
import Control.Exception.Lifted
import Network.Mime
import Network.HTTP.Types
import Network.Socks5
import Network.HTTP.Conduit hiding (Proxy)
import qualified Network.HTTP.Conduit as HTTP (Proxy)
import Network.HTTP.Conduit.Browser
import Control.Monad.Trans.Resource

import Text.HTML.TagSoup (Tag)
import Text.HTML.TagSoup.Fast

import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Text.Show

import Data.Conduit
import Data.Conduit.List as CL

import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

------------------------------------------------
-- | State and configuration shared by all units
data PerWipe
    = PerWipe
        {tpastagen :: !(TVar ((Int -> Blast ParsedThread) -> Maybe Page -> PostDest -> Blast ((Bool, ((Bool, Bool), String)))))
        ,timagegen :: !(TVar (Bool -> Blast (Bool, Image)))
        ,tuseimages :: !(TVar Bool)
        ,tappendjunkimages :: !(TVar Bool)
        ,tcreatethreads :: !(TVar Bool)
        ,tmakewatermark :: !(TVar Bool)
        ,tposttimeout :: !(TVar (Maybe Double))
        ,tthreadtimeout :: !(TVar (Maybe Double))
        ,tfluctuation :: !(TVar (Maybe Double))
        }

-- | State per board
data PerBoard
    = PerBoard
        {mthread :: !(TVar PostDest)
        ,mmode :: !(TVar (Maybe Mode))
        ,mposttimeout :: !(TVar (Maybe Double))
        ,mthreadtimeout :: !(TVar (Maybe Double))
        }

-- | State per proxy
data PerProxy
    = PerProxy
        {psharedCookies :: !(TMVar CookieJar)
        ,pcloudflareCaptchaLock :: !(TMVar ())
        ,pUserAgent :: !(TVar ByteString)
        }

mkEmptyPerProxy :: IO PerProxy
mkEmptyPerProxy = do
    psharedCookies <- atomically $ newEmptyTMVar
    pcloudflareCaptchaLock <- atomically $ newTMVar ()
    pUserAgent <- atomically . newTVar =<< chooseFromList userAgents
    return PerProxy{..}

data CaptchaType
    = CaptchaPosting
    | CaptchaCloudflare
  deriving Show

data LogType
    = LogNormal
    | LogDebug
  deriving (Eq, Show, Ord, Enum)

data CaptchaAnswer
    = Answer !String !(OriginStamp -> IO ())
    | ReloadCaptcha
    | AbortCaptcha

data Message
    = OutcomeMessage !Outcome
    | LogMessage !LogType !String
    | SupplyCaptcha
        {captchaType :: !CaptchaType
        ,captchaBytes :: !LByteString
        ,captchaSend :: !(CaptchaAnswer -> IO ())
        }
    | NoPastas
    | NoImages

data OutMessage
    = OutMessage !OriginStamp !Message

data LogDetail
    = LogDetail
        {logNormal :: !Bool
        ,logDebug :: !Bool
        }
  deriving (Eq, Show)

data BlastData
    = BlastData
        {bProxy :: !Proxy
        ,bBoard :: !Board
        ,bLogDetail :: !LogDetail
        ,bPerWipe :: !PerWipe
        ,bPerBoard :: !PerBoard
        ,bPerProxy :: !PerProxy
        ,bOut :: !(OutMessage -> IO ())
        }

type Blast = ReaderT BlastData (StateT OriginInfo BrowserAction)

instance Show CaptchaAnswer where
    show (Answer a _) = "Answer " ++ show a ++ " <repBad>"
    show ReloadCaptcha = "ReloadCaptcha"
    show AbortCaptcha = "AbortCaptcha"

instance Show Message where
    show (OutcomeMessage o) = show o
    show (LogMessage LogNormal o) = o
    show (LogMessage LogDebug o) = "<debug>" ++ o ++ "</debug>"
    show (SupplyCaptcha t _ _) = "SupplyCaptcha " ++ show t
    show NoPastas = "NoPastas"
    show NoImages = "NoImages"

instance Show OutMessage where
    show (OutMessage s m) = show s ++ ": " ++ show m

instance NFData LogType

instance NFData CaptchaType

instance NFData CaptchaAnswer where
    rnf (Answer s r) = r `seq` rnf s
    rnf _ = ()

instance NFData Message where
    rnf (OutcomeMessage o) = rnf o
    rnf (LogMessage d s) = rnf (d,s)
    rnf (SupplyCaptcha c b s) = rnf (c, b) `deepseq` s `seq` ()
    rnf _ = ()

instance NFData OutMessage where
    rnf (OutMessage os m) = rnf (os, m)

recMode :: Mode -> Blast ()
recMode m = lift get >>= \s -> lift $ put s{gmode=m}

recThread :: PostDest -> Blast ()
recThread t = lift get >>= \s -> lift $ put s{gthread=t}

{-# DEPRECATED genOriginStamp "FIXME" #-}
genOriginStamp :: Blast OriginStamp
genOriginStamp = do
    BlastData{..} <- ask
    OriginInfo{..} <- lift get
    now <- liftIO getZonedTime
    return $ OriginStamp now bProxy bBoard gmode gthread

sendOut :: Message -> Blast ()
sendOut msg = do
    stamp <- genOriginStamp
    let a = OutMessage stamp msg
    send <- asks bOut
    a `deepseq` liftIO (send a)

log :: String -> Blast ()
log msg = do
    d <- asks bLogDetail
    when (logNormal d) $ do
        sendOut (LogMessage LogNormal msg)

debug :: String -> Blast ()
debug msg = do
    d <- asks bLogDetail
    when (logDebug d) $ do
        sendOut (LogMessage LogDebug msg)

runBlast :: Manager -> Proxy -> BlastData -> Blast a -> IO a
runBlast m p bdata a =
    runResourceT $ browse m $ do
        flip evalStateT def $ flip runReaderT bdata $ do
            setupBlast p
            a

setupBlast :: Proxy -> Blast ()
setupBlast proxy = do
    httpSetProxy proxy
    userAgent <- liftIO . readTVarIO =<< asks (bPerProxy >>> pUserAgent)
    runHttp $ do
        setMaxRedirects Nothing
        setMaxRetryCount 1
        setTimeout $ Just $ 10 * 1000000
        setUserAgent $ Just userAgent
        setOverrideHeaders [(hAcceptLanguage, "ru;q=1.0, en;q=0.1")
                           ,(hConnection, "keep-alive")]
        --
        --setCookieFilter $ \_ _ -> return False

{-# DEPRECATED runHttp "FIXME" #-}
runHttp :: BrowserAction a -> Blast a
runHttp = lift . lift

httpSetProxies :: Maybe HTTP.Proxy -> Maybe SocksConf -> Blast ()
httpSetProxies h s = runHttp $ do
    setCurrentProxy h
    setCurrentSocksProxy s

httpSetProxy :: Proxy -> Blast ()
httpSetProxy NoProxy = httpSetProxies Nothing Nothing
httpSetProxy (HttpProxy p) = httpSetProxies (Just p) Nothing
httpSetProxy (SocksProxy p) = httpSetProxies Nothing (Just p)

httpGetProxy :: Blast Proxy
httpGetProxy = runHttp $ do
    h <- getCurrentProxy
    case h of
        Just p -> return $ HttpProxy p
        Nothing -> do
            s <- getCurrentSocksProxy
            case s of
                Just p -> return $ SocksProxy p
                Nothing -> return NoProxy

httpWithProxy :: Proxy -> Blast a -> Blast a
httpWithProxy p m = bracket
    httpGetProxy
    (\current -> httpSetProxy current)
    (\_ -> do
        httpSetProxy p
        m)

httpReq :: Request (ResourceT IO) -> Blast (Response (ResumableSource (ResourceT IO) ByteString))
httpReq = runHttp . makeRequest

httpReqLbs :: Request (ResourceT IO) -> Blast (Response LByteString)
httpReqLbs = runHttp . makeRequestLbs

httpReqStr :: Request (ResourceT IO) -> Blast (Response Text)
httpReqStr u = do
    x <- httpReq u
    liftResourceT $ fmap ((<$ x) . T.concat) $ responseBody x $$+- CL.map T.decodeUtf8 =$ consume

httpReqStrTags :: Request (ResourceT IO) -> Blast (Response [Tag Text])
httpReqStrTags u = do
    x <- httpReq u
    liftResourceT $ fmap ((<$ x) . parseTagsT . S.concat) $ responseBody x $$+- consume

httpGet :: String -> Blast (Response (ResumableSource (ResourceT IO) ByteString))
httpGet u = runHttp $ do
    r <- parseUrl u
    makeRequest r

httpGetLbs :: String -> Blast LByteString
httpGetLbs u = runHttp $ do
    r <- parseUrl u
    responseBody <$> makeRequestLbs r

httpGetStr :: String -> Blast Text
httpGetStr u = do
    g <- httpGet u
    let x = liftResourceT $ fmap T.concat $ responseBody g $$+- CL.map T.decodeUtf8 =$ consume
    x

httpGetStrTags :: String -> Blast [Tag Text]
httpGetStrTags u = do
    g <- httpGet u
    let x = liftResourceT $ fmap (parseTagsT . S.concat) $ responseBody g $$+- consume
    x
