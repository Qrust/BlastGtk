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
import BlastItWithPiss.Proxy
import Control.Exception.Lifted
import Network.Mime
import Network.HTTP.Types
import Network.Socket.Internal
import Network.Socks5
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Control.Monad.Trans.Resource

import Text.HTML.TagSoup (Tag)
import Text.HTML.TagSoup.Fast

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Text.Show as Show

import Data.Conduit
import Data.Conduit.List as CL

import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

-- HACK unsafePerformIO
import qualified System.IO.Unsafe as Unsafe (unsafePerformIO)
{-# NOINLINE userAgent #-}
userAgent :: ByteString
userAgent = Unsafe.unsafePerformIO $ chooseFromList userAgents

type Blast = BrowserAction

------------ORIGIN STAMP----------------------
data OriginStamp
    = OriginStamp
        {oTime :: !ZonedTime
        ,oProxy :: !BlastProxy
        ,oBoard :: !Board
        ,oMode :: !Mode
        ,oThread :: !PostDest
        }

renderCompactStamp :: OriginStamp -> String
renderCompactStamp (OriginStamp _ proxy board _ _) =
    renderBoard board ++ " {" ++ show proxy ++ "}"

instance Show OriginStamp where
    show (OriginStamp time proxy board mode thread) =
        "(" ++ show time ++ ") " ++ "{" ++ show proxy ++ "} " ++ renderBoard board ++
        " " ++ show mode ++ " [| " ++
        ssachThread board thread ++ " |]"
------------------------------------------------
-- | State and configuration shared by all units
data PerWipe
    = PerWipe
        {tpastagen :: !(TVar ((Int -> BlastLog ParsedThread) -> Maybe Page -> PostDest -> BlastLog ((Bool, ((Bool, Bool), String)))))
        ,timagegen :: !(TVar (Bool -> BlastLog (Bool, Image)))
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

mkEmptyPerBoard :: IO PerBoard
mkEmptyPerBoard = atomically $
    PerBoard <$> newTVar NewThread <*> newTVar Nothing <*> newTVar Nothing <*> newTVar Nothing

-- | State per proxy
data PerProxy
    = PerProxy
        {psharedCookies :: !(TMVar CookieJar)
        ,pcloudflareCaptchaLock :: !(TMVar ())
        }

mkEmptyPerProxy :: IO PerProxy
mkEmptyPerProxy = atomically $ do
    psharedCookies <- newEmptyTMVar
    pcloudflareCaptchaLock <- newTMVar ()
    return PerProxy{..}

data CaptchaType
    = CaptchaPosting
    | CaptchaCloudflare

data CaptchaAnswer
    = Answer !String !(OriginStamp -> IO ())
    | ReloadCaptcha
    | AbortCaptcha

data Message
    = OutcomeMessage !Outcome
    | LogMessage !String
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

data BlastLogData
    = BlastLogData
        {bldProxy :: !BlastProxy
        ,bldBoard :: !Board
        ,bldLogD :: !LogDetail
        ,bldShS :: !PerWipe
        ,bldMuS :: !PerBoard
        ,bldPrS :: !PerProxy
        ,bldOut :: !(OutMessage -> IO ())
        }

data OriginInfo
    = OriginInfo
        {gmode :: !Mode
        ,gthread :: !PostDest
        }

type BlastLog
    = ReaderT BlastLogData (StateT OriginInfo Blast)

instance Show CaptchaAnswer where
    show (Answer a _) = "Answer " ++ show a ++ " <repBad>"
    show ReloadCaptcha = "ReloadCaptcha"
    show AbortCaptcha = "AbortCaptcha"

instance Show Message where
    show (OutcomeMessage o) = show o
    show (LogMessage o) = o
    show SupplyCaptcha{} = "SupplyCaptcha"
    show NoPastas = "NoPastas"
    show NoImages = "NoImages"

instance Show OutMessage where
    show (OutMessage s m) = show s ++ ": " ++ show m

-- HACK Default OriginInfo
instance Default OriginInfo where
    def = OriginInfo CreateNew NewThread

instance NFData CaptchaType

instance NFData CaptchaAnswer where
    rnf (Answer s r) = r `seq` rnf s
    rnf _ = ()

instance NFData OriginStamp where
    rnf (OriginStamp t p b m th) = rnf (t,p,b,m,th)

instance NFData Message where
    rnf (OutcomeMessage o) = rnf o
    rnf (LogMessage s) = rnf s
    rnf (SupplyCaptcha c b s) = rnf (c, b) `deepseq` s `seq` ()
    rnf _ = ()

instance NFData OutMessage where
    rnf (OutMessage os m) = os `deepseq` m `deepseq` ()

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
