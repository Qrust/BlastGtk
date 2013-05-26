-- | Kludge: the main module
module BlastItWithPiss
    (ShSettings(..)
    ,MuSettings(..)
    ,ProxySettings

    ,LogDetail(..)
    ,SageMode(..)

    ,CaptchaType(..)
    ,CaptchaAnswer(..)

    ,OriginStamp(..)
    ,renderCompactStamp

    ,SupplyCaptcha(..)

    ,Message(..)
    ,OutMessage(..)

    ,TempGenType
    ,mkFullGen
    ,mkIgnoreGen
    ,mkConstGen

    ,defMuS
    ,defPrS
    ,entryPoint
    ,sortSsachBoardsByPopularity

    -- FIXME
    ,TempBlastPastaChannel(..)
    ) where
import Import

import BlastItWithPiss.Blast
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Post

import Text.Recognition.Antigate

import qualified Data.Text as T

import Control.Concurrent.Lifted
import Control.Concurrent.STM hiding (readTVarIO)
import qualified Control.Concurrent.STM as STM

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Resource

import Data.Time.Clock.POSIX

import qualified Text.Show as Show
import Text.HTML.TagSoup(Tag)

import System.IO (putStrLn, putStr)




-- TVar (IntMap (TMVar Page)) ? TVar (IntMap (TMVar (TMVar Page)))

-- agent can also fail to get a page, same thing as blastCloudflare's lock really.

-- If got no page in five seconds, attempt to fetch on your own.
--  use registerDelay to set timeout

-- if no key in IntMap?

-- periodically purge redownload pages, use max one agent (how?)

-- use proxies with best response time

-- board threads?

#ifdef TEST
import qualified Data.IntMap as I

mockup :: TVar (I.IntMap (TMVar Page)) -> Int -> IO ()
mockup tpageCache pid = do
    ttimeup <- registerDelay (5&millions)
    atomically $ do
        pageCache <- readTVar tpageCache
        case I.lookup pid pageCache of
            Just waitPage -> do
                x <- Just <$> readTMVar waitPage
                  <|> Nothing <$ check <$> readTVar ttimeup
                case x of
                  Just page -> return ()
                  Nothing   -> error "ahadhadjsfj"
            Nothing -> error "lakjhoiadghoiasjgha"

#endif

{-# INLINE readTVarIO #-}
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO
{- we only read tvars.

{-# INLINE atomically #-}
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically
-}
-- | Global
data ShSettings = ShSettings
    -- NOTE HACK FIXME all browser state accumulated in *gens is lost.
    -- NOTE HACK FIXME *gens do not bypass cloudflare
    {tpastagen         :: !(TVar (TempGenType TempBlastPastaChannel))
    ,timagegen         :: !(TVar (TempGenType Image))
    ,tvideogen         :: !(TVar (TempGenType Text))

    ,tuseimages        :: !(TVar Bool)
    ,tappendjunkimages :: !(TVar Bool)
    ,tmakewatermark    :: !(TVar Bool)

    ,tcreatethreads    :: !(TVar Bool)
    ,tsagemode         :: !(TVar SageMode)

    ,tposttimeout      :: !(TVar (Maybe Double))
    ,tthreadtimeout    :: !(TVar (Maybe Double))
    ,tfluctuation      :: !(TVar (Maybe Double))
    }

-- | PerBoard
data MuSettings = MuSettings
    {mthread        :: !(TVar (Maybe Int))
    ,mmode          :: !(TVar (Maybe Mode))
    ,mposttimeout   :: !(TVar (Maybe Double))
    ,mthreadtimeout :: !(TVar (Maybe Double))
    }

-- | PerProxy
data ProxySettings = ProxyS
    {pSharedCookies :: !(MVar (MVar (Maybe CookieJar)))
    ,pUserAgent     :: !UserAgent
    }

defMuS :: IO MuSettings
defMuS = do
    mthread <- newTVarIO Nothing
    mmode <- newTVarIO Nothing
    mposttimeout <- newTVarIO Nothing
    mthreadtimeout <- newTVarIO Nothing
    return MuSettings{..}

defPrS :: IO ProxySettings
defPrS = do
    pSharedCookies <- newEmptyMVar
    pUserAgent <- newUserAgent
    return ProxyS{..}

data CaptchaType
    = CaptchaPosting
    | CaptchaCloudflare

data CaptchaAnswer
    = Answer !String !(OriginStamp -> IO ())
    | ReloadCaptcha
    | AbortCaptcha

data OriginStamp = OriginStamp
    {oTime   :: !ZonedTime
    ,oProxy  :: !BlastProxy
    ,oBoard  :: !Board
    ,oMode   :: !Mode
    ,oThread :: !(Maybe Int)
    }

data SupplyCaptcha = SupplyCaptcha
    {captchaType     :: !CaptchaType
    ,captchaBytes    :: !LByteString
    ,captchaSend     :: !(CaptchaAnswer -> IO ())
    ,captchaConf     :: !CaptchaConf
    ,captchaFilename :: !String
    }

data Message
    = OutcomeMessage !Outcome
    | LogMessage !Text
    | SolveCaptcha !SupplyCaptcha
    | NoPastas
    | NoImages

data OutMessage = OutMessage !OriginStamp !Message

data LogDetail
    = Log
    | Don'tLog
  deriving (Eq, Show, Ord, Enum, Bounded)

data BlastLogData = BlastLogData
    {bProxy       :: !BlastProxy
    ,bBoard       :: !Board
    ,bLogD        :: !LogDetail
    ,bShS         :: !ShSettings
    ,bMuS         :: !MuSettings
    ,bPrS         :: !ProxySettings
    ,bOut         :: !(OutMessage -> IO ())
    ,bOtherFields :: ![Part Blast (ResourceT IO)] -- ^ Kludge
    }

data OriginInfo = OriginInfo
    {gmode   :: !Mode
    ,gthread :: !(Maybe Int)
    }

data BlastLogState = BlastLogState
    {bsOriginInfo              :: !OriginInfo
    ,bsAdaptivityIn            :: !Bool
    ,bsOptimisticLastPostTime  :: !POSIXTime
    ,bsPessimisticLastPostTime :: !POSIXTime
    ,bsLastThreadTime          :: !POSIXTime -- ^ Always pessimistic
    }

instance Default BlastLogState where
    def = BlastLogState def False 0 0 0

type BlastLog = ReaderT BlastLogData (StateT BlastLogState Blast)

instance Show CaptchaAnswer where
    show (Answer a _) = "Answer " ++ show a ++ " <repBad>"
    show ReloadCaptcha = "ReloadCaptcha"
    show AbortCaptcha = "AbortCaptcha"

renderCompactStamp :: OriginStamp -> Text
renderCompactStamp (OriginStamp _ proxy board _ _) =
    renderBoard board ++ " {" ++ show proxy ++ "}"

instance Show OriginStamp where
    show (OriginStamp time proxy board mode thread) =
        "(" ++ show time ++ ") " ++ "{" ++ show proxy ++ "} " ++ renderBoard board ++
        " " ++ show mode ++ " [| " ++
        ssachThread board thread ++ " |]"

instance Show Message where
    show (OutcomeMessage o) = show o
    show (LogMessage o) = T.unpack o
    show SolveCaptcha{} = "SolveCaptcha"
    show NoPastas = "NoPastas"
    show NoImages = "NoImages"

instance Show OutMessage where
    show (OutMessage s m) = show s ++ ": " ++ show m

instance Default OriginInfo where
    def = OriginInfo CreateNew Nothing

instance NFData CaptchaType

instance NFData CaptchaAnswer where
    rnf (Answer s r) = r `seq` rnf s
    rnf _ = ()

instance NFData OriginStamp where
    rnf (OriginStamp t p b m th) = rnf (t,p,b,m,th)

instance NFData SupplyCaptcha where
    rnf (SupplyCaptcha a b c d e) = rnf(a,b,c,d,e)

instance NFData Message where
    rnf (OutcomeMessage o) = rnf o
    rnf (LogMessage s) = rnf s
    rnf (SolveCaptcha s) = rnf s
    rnf _ = ()

instance NFData OutMessage where
    rnf (OutMessage os m) = os `deepseq` m `deepseq` ()

data SageMode
    = SageDisabled
    | SageEnabled
    | SageAccordingToMode
  deriving (Eq, Show)

newtype TempGenType a =
    TempGenType ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO a)

mkFullGen :: ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO a) -> TempGenType a
mkFullGen = TempGenType

mkIgnoreGen :: IO a -> TempGenType a
mkIgnoreGen = TempGenType . const . const . const

mkConstGen :: a -> TempGenType a
mkConstGen = mkIgnoreGen . return

evalGen :: TempGenType a -> (Int -> IO Thread) -> Maybe Page -> Maybe Int -> BlastLog a
evalGen (TempGenType gen) ioGetThread mpage thread = do
    liftIO $ gen ioGetThread mpage thread

fromSageMode :: SageMode -> Mode -> Bool
fromSageMode SageDisabled _ = False
fromSageMode SageEnabled _ = True
fromSageMode SageAccordingToMode m = obligatorySageMode m

blastPostTimeout :: BlastLog POSIXTime
blastPostTimeout = do
    BlastLogData {
      bBoard = board
    , bShS = ShSettings
        { tposttimeout }
    , bMuS = MuSettings
        { mposttimeout }
    } <- ask

    (fmap realToFrac <$> readTVarIO mposttimeout) >>= fromMaybeM (
        (fmap realToFrac <$> readTVarIO tposttimeout) >>= fromMaybeM (
            return $ ssachPostTimeout board)
        )

blastThreadTimeout :: BlastLog POSIXTime
blastThreadTimeout = do
    BlastLogData {
      bBoard = board
    , bShS = ShSettings
        { tthreadtimeout }
    , bMuS = MuSettings
        { mthreadtimeout }
    } <- ask

    (fmap realToFrac <$> readTVarIO mthreadtimeout) >>= fromMaybeM (
        (fmap realToFrac <$> readTVarIO tthreadtimeout) >>= fromMaybeM (
            return $ ssachThreadTimeout board)
        )

runBlastLog :: BlastLogData -> BlastLog a -> Blast a
runBlastLog d m = evalStateT (runReaderT m d) def

runBlastLogSt :: BlastLogData -> BlastLogState -> BlastLog a -> Blast a
runBlastLogSt d o m = evalStateT (runReaderT m d) o

blast :: Blast a -> BlastLog a
blast = lift . lift

askBoard :: BlastLog Board
askBoard = asks bBoard

setAdaptive :: Bool -> BlastLog ()
setAdaptive n = lift $ get >>= \s -> put s{bsAdaptivityIn=n}

recMode :: Mode -> BlastLog ()
recMode m = lift get >>= \s@BlastLogState{..} -> lift $ put s{bsOriginInfo=bsOriginInfo{gmode=m}}

recThread :: (Maybe Int) -> BlastLog ()
recThread t = lift get >>= \s@BlastLogState{..} -> lift $ put s{bsOriginInfo=bsOriginInfo{gthread=t}}

setLastThreadTime :: POSIXTime -> BlastLog ()
setLastThreadTime t = lift get >>= \s -> lift $ put s{bsLastThreadTime=t}

setOptimisticLastPostTime :: POSIXTime -> BlastLog ()
setOptimisticLastPostTime t = lift get >>= \s -> lift $ put s{bsOptimisticLastPostTime=t}

setPessimisticLastPostTime :: POSIXTime -> BlastLog ()
setPessimisticLastPostTime t = lift get >>= \s -> lift $ put s{bsPessimisticLastPostTime=t}

genOriginStamp :: BlastLog OriginStamp
genOriginStamp = do
    BlastLogData {
      bBoard
    , bProxy
    } <- ask

    BlastLogState{
     bsOriginInfo = OriginInfo
        { gmode
        , gthread
        }
    } <- lift get

    now <- liftIO getZonedTime

    return $ OriginStamp now bProxy bBoard gmode gthread

blastOut :: Message -> BlastLog ()
blastOut msg = do
    BlastLogData{..} <- ask

    stamp <- genOriginStamp

    liftIO $ bOut $!! OutMessage stamp msg

blastLog :: Text -> BlastLog ()
blastLog msg = do
    BlastLogData{..} <- ask

    case bLogD of
      Log -> blastOut $ LogMessage msg
      _ -> return ()

-- HACK abortWithOutcome
data AbortOutcome = AbortOutcome !Outcome
    deriving (Show, Typeable)

instance Exception AbortOutcome

abortWithOutcome :: Outcome -> BlastLog a
abortWithOutcome o = do
    blastLog $ "Aborting with outcome: " ++ show o
    blastOut $ OutcomeMessage o
    throwIO  $ AbortOutcome   o
-- /HACK

data TempBlastPastaChannel
    = TBPC
        {escinv :: !Bool
        ,escwrd :: !Bool
        ,pasta  :: !String
        }

blastPostData :: Mode -> Maybe Page -> Maybe Int -> BlastLog PostData
blastPostData mode mpastapage thread = do
    BlastLogData {
      bShS = ShSettings
        { tpastagen
        , timagegen
        , tvideogen

        , tuseimages
        , tappendjunkimages

        , tsagemode

        , tmakewatermark
        }
    } <- ask

    blastLog "Choosing pasta..."

    _readerState <- ask
    _stateState <- lift get
    _blastState <- blast getBlastState
    _manager <- blast getManager

    let _getThread =
            runBlast _manager _blastState
              . runBlastLogSt _readerState _stateState
                . getThread

    let {-# INLINE runGen #-}
        runGen :: TempGenType a -> BlastLog a
        runGen g = evalGen g _getThread mpastapage thread

    TBPC{..} <- do
        pastagen <- readTVarIO tpastagen
        runGen pastagen
    blastLog $
        "chose pasta, escaping invisibles " ++ show escinv ++
        ", escaping wordfilter " ++ show escwrd ++ ": \"" ++
        fromString pasta ++ "\""

    sagemode <- readTVarIO tsagemode
    let sage = fromSageMode sagemode mode
    blastLog $ "Sage mode: " ++ show sagemode
            ++ ", Sage: " ++ show sage

    blastLog "Choosing image"
    cleanImage <- do
        enableImages <- readTVarIO tuseimages
        if (enableImages && not sage) || null pasta || obligatoryImageMode mode
          then do
            -- eval imageGen
            imagegen <- readTVarIO timagegen
            Just <$> runGen imagegen
          else
            return Nothing

    junkingEnabled <- readTVarIO $ tappendjunkimages
    junkImage <-
        case cleanImage of
          Nothing -> do
            return Nothing
          Just i -> Just <$> do
            if junkingEnabled
              then
                appendJunk i
              else
                return $ JunkImage i

    case junkImage of
      Nothing -> blastLog "chose no image"
      Just i -> blastLog $
        "chose image \"" ++ fromString (filename $ fromJunkImage i)
        ++ "\", junk: " ++ show junkingEnabled

    blastLog $ "Choosing video"
    video <- runGen =<< readTVarIO tvideogen
    blastLog $ "Chose video: " ++ video

    watermark <- readTVarIO tmakewatermark
    blastLog $ "Watermark: " ++ show watermark

    let final = PostData
                {subject = ""
                ,text = pasta
                ,image = junkImage
                ,video = video
                ,sage = fromSageMode sagemode mode
                ,makewatermark = watermark
                ,escapeInv = escinv
                ,escapeWrd = escwrd}
    final `deepseq` return final

blastCaptcha :: Maybe Int -> BlastLog (Bool, Maybe (CAnswer Blast (ResourceT IO), (OriginStamp -> IO ())))
blastCaptcha thread = do
    board <- askBoard

    blastLog "Fetching challenge"

    mbbytes <- blast $ getNewCaptcha board thread ""
    case mbbytes of
      Left f -> do
        blastLog $ "Got presolved captcha " ++ show f
        return (False, Just (f, const $ return ()))
      Right (chKey :: CurrentSsachCaptchaType) -> do
        blastLog "Downloading captcha"

        (bytes, ct) <- blast $ getCaptchaImage chKey
        cconf <- blast $ getCaptchaConf chKey
        fname <- mkImageFileName ct

        blastLog "Got captcha image, sending captcha mvar"

        m <- newEmptyMVar
        blastOut $ SolveCaptcha $
            SupplyCaptcha CaptchaPosting bytes (putMVar m $!!) cconf fname
        blastLog "blocking on captcha mvar"
        a <- takeMVar m

        blastLog $ "got captcha mvar, answer is... " ++ show a
        case a of
          Answer s r -> do
            f <- blast $ applyCaptcha chKey s
            return (True, Just (f, r))
          ReloadCaptcha -> blastCaptcha thread
          AbortCaptcha -> return (True, Nothing)

-- FIXME Code duplication with "post" (checking cloudflare)
-- FIXME Code duplication with "blastCaptcha" (fetching captcha)
-- FIXME what about strict checkStatus in request?
blastCloudflare
    :: (Response [Tag Text] -> BlastLog b)
        -- ^ after we're done, feed the decloudflared page to this
    -> (BlastLog (Response [Tag Text]), String)
        -- ^ fst: getter for a page possibly protected with cloudflare.
        --   snd: Url of that page, so we can post captcha answer to it
    -> BlastLog b
blastCloudflare continueWith (getCloudyPage, url) = do

    blastLog "Fetching page for cloudflare inspection"
    pg <- getCloudyPage

    blastLog "Inspecting"
    blastCloudflare' pg

  where

    blastCloudflare' rsp
        | responseStatus rsp == status404 && (maybe False (== "NWS_QPLUS_HY") $
            lookup hServer $ responseHeaders rsp) = do
            abortWithOutcome Four'o'FourBan -- HACK HACK HACK oyoyoyoy
        | responseStatus rsp == status403 && cloudflareBan (responseBody rsp) = do
            abortWithOutcome CloudflareBan -- HACK HACK HACK oyoyoyoy
        | responseStatus rsp == status403 && cloudflareCaptcha (responseBody rsp) = do
            blastLog "Encountered cloudflare challenge"
            cloudflareChallenge
        | otherwise = do
            blastLog "blastCloudflare: No cloudflare spotted..."
            continueWith rsp

    cloudflareChallenge = do
        eitherJobCookies <- takeCloudflareLock

        case eitherJobCookies of
          Left maybeCookiesMVar -> do
            solveCloudflareCaptcha maybeCookiesMVar

            cloudflareChallenge
          Right cookies -> do
            blastLog "Got cloudflare cookies"

            blast $ setCookieJar cookies

            continueWith =<< getCloudyPage

    takeCloudflareLock = do
        pSharedCookies <- asks $ pSharedCookies . bPrS

        maybeMaybeCookiesMVar <- tryTakeMVar pSharedCookies
        case maybeMaybeCookiesMVar of
          Nothing -> do
            maybeCookiesMVar <- newEmptyMVar
            myJob <- tryPutMVar pSharedCookies maybeCookiesMVar

            if myJob
              then
                return $ Left maybeCookiesMVar
              else do
                blastLog "Someone is already solving challenge, retrying."
                takeCloudflareLock
          Just oldMaybeCookiesMVar -> do
            blastLog "Waiting for cloudflare cookies..."

            _maybeCookies <- readMVar oldMaybeCookiesMVar
            case _maybeCookies of
              _v@(Just cookies) -> do
                putMVar pSharedCookies oldMaybeCookiesMVar

                return $ Right cookies
              Nothing -> do
                newMaybeCookiesMVar <- newEmptyMVar
                putMVar pSharedCookies newMaybeCookiesMVar

                blastLog "Previous agent failed to solve cloudflare"
                return $ Left newMaybeCookiesMVar

    solveCloudflareCaptcha maybeCookiesMVar =
        (do mcookies <- reallySolveCloudflareCaptcha
            case mcookies of
              Just cookies -> do
                putMVar maybeCookiesMVar (Just cookies)
                let !cookieCount = length (destroyCookieJar cookies)
                blastLog $ "Cloudflare cookie count: " ++ show cookieCount
              Nothing -> do
                putMVar maybeCookiesMVar Nothing
        ) `catches`
            [Handler $ \(e::AsyncException) -> do
                putMVar maybeCookiesMVar Nothing
                blastLog "Async exception aborted cloudflare challenge solving"
                throwIO e
            ,Handler $ \(e::SomeException) -> do
                putMVar maybeCookiesMVar Nothing
                blastLog $
                    "Exception aborted cloudflare challenge solving"
                    ++ ", exception was: " ++ show e
            ]

    reallySolveCloudflareCaptcha = do
        blastLog "locked cloudflare captcha"

        chKey <- blast $ recaptchaChallengeKey cloudflareRecaptchaKey

        (bytes, ct) <- blast $ getCaptchaImage $ Recaptcha chKey
        fname <- mkImageFileName ct

        cconf <- blast $ getCaptchaConf $ Recaptcha chKey

        captchaMVar <- newEmptyMVar
        blastOut $ SolveCaptcha $ SupplyCaptcha
                {captchaType = CaptchaCloudflare
                ,captchaBytes = bytes
                ,captchaSend = \a -> a `deepseq` putMVar captchaMVar a
                ,captchaConf = cconf
                ,captchaFilename = fname
                }
        answer <- takeMVar captchaMVar

        case answer of
          Answer s _ -> do
            let
              _rq = (fromJust $ parseUrl url)
                {checkStatus = \_ _ _ -> Nothing
                ,redirectCount = 0}
              rq =
                [("recaptcha_challenge_key", fromString chKey)
                ,("recaptcha_response_key", fromString s)
                ,("message", "")
                ,("act", "captcha")
                ] `urlEncodedBody` _rq

            _ <- blast $ httpReq rq

            ck <- blast getCookieJar

            if null (destroyCookieJar ck)
              then do
                blastLog "CLOUDFLARE ERROR: Empty cookie jar, invalid captcha?"
                return Nothing
              else do
                blastLog "finished working on captcha"
                return (Just ck)
          ReloadCaptcha ->
            return Nothing
          AbortCaptcha ->
            return Nothing

blastPost
    :: POSIXTime
    -> Bool
    -> [Part Blast (ResourceT IO)]
    -> Mode
    -> Maybe Int
    -> PostData
    -> BlastLog ()
blastPost threadtimeout captchaNeeded otherfields mode thread postdata = do

    BlastLogData {
      bBoard = board
    , bShS = ShSettings{..}
    , bMuS = MuSettings{..}
    } <- ask

    BlastLogState {
      bsAdaptivityIn = adaptive
    , ..
    } <- lift get

    (neededcaptcha, mcap) <-
        if captchaNeeded || mode==CreateNew || not adaptive
          then do
            blastLog "querying captcha"
            blastCaptcha thread
          else do
            blastLog "skipping captcha"
            return (False, Just (def, const $ return ()))
    case mcap of
        Nothing -> blastLog "Refused to solve captcha, aborting post"
        Just (captcha, reportbad) -> do

            p <- blast $ prepare board thread postdata captcha
                            otherfields ssachLengthLimit

            posttimeout <- blastPostTimeout
            blastLog $ "Post timeout: " ++ show posttimeout
            blastLog $ "Thread timeout: " ++ show threadtimeout

            mfluctuation <- readTVarIO tfluctuation
            blastLog $ "Post time fluctuation: " ++ show mfluctuation

            canDefinitelyPost <-
                flip fmap (liftIO getPOSIXTime) $ \now ->
                    now - bsPessimisticLastPostTime >= posttimeout

            -- implying that we get CreateNew only when we definitely can post
            unless (mode == CreateNew || canDefinitelyPost) $ do
                !fluctuation <- flip (maybe $ return 0) mfluctuation $ \f -> do
                    lowerHalf <- chooseFromList [True, True, True, False]
                    r <- realToFrac <$> getRandomR
                        (if lowerHalf then 0 else f/2, if lowerHalf then f/2 else f)
                    blastLog $ "Sleep added by fluctuation: " ++ show r
                    return r

                let !orientedLastPostTime =
                        if adaptive
                          then bsOptimisticLastPostTime
                          else bsPessimisticLastPostTime

                now <- liftIO getPOSIXTime
                let slptime =
                        (orientedLastPostTime + posttimeout)
                        - now + fluctuation

                blastLog $ "sleeping " ++ show slptime ++
                    " seconds before post. FIXME using threadDelay for sleeping"
                    ++ ", instead of a more precise timer"
                threadDelay $ toMicroseconds slptime

            blastLog "posting"

            beforePost <- liftIO getPOSIXTime -- optimistic
            (out, _) <- blast $ post p
            afterPost <- liftIO getPOSIXTime -- pessimistic

            blastOut (OutcomeMessage out)

            when (successOutcome out) $ do
                when (neededcaptcha && adaptive) $ setAdaptive False
                unless (neededcaptcha || adaptive) $ setAdaptive True
                if mode == CreateNew
                  then do
                    setLastThreadTime afterPost
                  else do
                    setPessimisticLastPostTime afterPost
                    setOptimisticLastPostTime beforePost
                blastLog "post succeded"

            case out of
                Success -> return ()
                SuccessLongPost rest ->
                    when (mode /= CreateNew) $ do
                        blastPost threadtimeout captchaNeeded otherfields mode thread $
                            postdata{text = rest, image = Nothing}
                TooFastThread -> do
                    let m = threadtimeout / 3
                    blastLog $ "TooFastThread, retrying in " ++ show (m/60) ++ " minutes"
                    setLastThreadTime $ beforePost - m
                PostRejected -> do
                    blastLog "PostRejected, retrying later..."
                o | o==NeedCaptcha || o==WrongCaptcha -> do
                    blastLog $ show o ++ ", requerying"
                    liftIO . reportbad =<< genOriginStamp
                    blastPost threadtimeout True otherfields mode thread postdata
                  | otherwise -> do
                    if o==TooFastPost
                      then
                        blastLog "TooFastPost, retrying in 0.5 seconds"
                      else
                        blastLog "post failed, retrying in 0.5 seconds"
                    setOptimisticLastPostTime $ beforePost - (posttimeout - 0.5)
                    setPessimisticLastPostTime $ beforePost - (posttimeout - 0.5)

blastLoop :: BlastLog ()
blastLoop =
    forever $ do

    BlastLogData {
      bBoard = board
    , bOtherFields
    , bShS = ShSettings
        { tcreatethreads
        }
    , bMuS = MuSettings
        { mmode
        , mthread
        }
    } <- ask

    BlastLogState
        { bsLastThreadTime
        } <- lift get

    now <- liftIO getPOSIXTime
    threadtimeout <- blastThreadTimeout
    canmakethread <-
        ifM (readTVarIO tcreatethreads)
          (return $ now - bsLastThreadTime >= threadtimeout)
          (return False)

    mp0 <- do
        threadIsSet <- isJust <$> readTVarIO mthread
        if threadIsSet
          then
            return Nothing
          else Just <$> do
            blastLog "mp0: Getting first page."
            getPage 0

    case mp0 of
      Nothing -> blastLog "Thread chosen, ommitting page parsing"
      Just p0 -> blastLog $
        "page params:\n" ++
        "page id: " ++ show (pageId p0) ++ "\n" ++
        "lastpage id: " ++ show (lastpage p0) ++ "\n" ++
        "speed: " ++ show (speed p0) ++ "\n" ++
        "threads: " ++ show (length $ threads p0) ++ "\n" ++
        "max replies: " ++ maybe "COULDN'T PARSE THREADS, EXPECT CRASH IN 1,2,3..." show (maximumMay $ map postcount $ threads p0)

    mode <- readTVarIO mmode >>=
        maybe
        (do case mp0 of
              Nothing -> do
                blastLog "No page, throwing a dice for mode..."
                chooseFromList [minBound .. maxBound]
              Just p0 -> do
                blastLog "Choosing mode..."
                chooseMode board canmakethread p0)
        (\m -> do
            blastLog $ "Got mmode " ++ show m
            return m)
    recMode mode
    blastLog $ "chose mode " ++ show mode

    (thread, mpastapage) <-
        readTVarIO mthread >>= maybe
            (do blastLog "Choosing thread..."
                second Just <$>
                    chooseThread
                        board
                        mode
                        getPage
                        (fromMaybe (error "Page is Nothing while thread not specified") mp0))
            (\t -> do
                blastLog $ "Got mthread " ++ show t
                return (Just t, Nothing))
    recThread thread
    blastLog $ "chose thread " ++ show thread

    postdata <- blastPostData mode mpastapage thread

    blastPost threadtimeout False bOtherFields mode thread postdata

getPage :: Int -> BlastLog Page
getPage p = do
    board <- askBoard

    let url = ssachPage board p
        !req = (fromJust $ parseUrl url)
            {checkStatus = \_ _ _ -> Nothing}

    blastLog $ "getPage: going to page " ++ show p

    blastCloudflare
        (return . parsePage board . responseBody)
        (blast $ httpReqStrTags req, url)

getThread :: Int -> BlastLog Thread
getThread i = do
    board <- askBoard

    blastLog $ "Going into " ++ show i ++ " thread for pasta"
    blast $ headNote "PastaHead fail" . fst . parseThreads <$>
            httpGetStrTags (ssachThread board (Just i))

-- | Entry point should always be forked.
--
-- > thread <- forkIO (entryPoint print sh to Board ms)
--
-- You might want to resurrect thread if it dies.
--
-- > st <- threadStatus thread
-- > if st==ThreadDied || st==ThreadFinished
-- >    then resurrect
-- >    else continue
entryPoint
    :: Manager
    -> BlastProxy
    -> Board
    -> LogDetail
    -> ShSettings
    -> MuSettings
    -> ProxySettings
    -> (OutMessage -> IO ())
    -> IO ()
entryPoint manager proxy board lgDetail shS muS prS output =
    runBlastNew manager proxy (pUserAgent prS) $
    runBlastLog BlastLogData {
          bProxy = proxy
        , bBoard = board
        , bLogD = lgDetail
        , bShS = shS
        , bMuS = muS
        , bPrS = prS
        , bOut = output
        , bOtherFields = ssachLastRecordedFields board
        }
      $ do
        blastLog "Entry point"
        let hands =
              [Handler $ \(a::AsyncException) -> do
                blastLog $ "Got async " ++ show a
                throwIO a
              ,Handler $ \(_::AbortOutcome) -> do
                blastLog "HACK abortOutcome"
                -- wait what? {- start -} --HACK abortOutcome
              {-,Handler $ \(a::HttpException) -> do
                blastLog $ "Got http exception, restarting. Exception was: " ++ show a
                start -- Dunno what to do except restart.-}
              ,Handler $ \(a::SomeException) -> do
                blastLog $ "Terminated by exception " ++ show a
                blastOut $ OutcomeMessage $ InternalError $ ErrorException a
              ]
            start = flip catches hands $ blastLoop
        start

sortSsachBoardsByPopularity :: [Board] -> IO ([(Board, Int)], [Board])
sortSsachBoardsByPopularity boards = do
    ua <- newUserAgent
    bracket (newManager def) closeManager $ \m -> runBlastNew m NoProxy ua $ do
        maybeb <- forM boards $ \b -> do
            liftIO $ putStr $ "Processing " ++ renderBoard b ++ ". Speed: "
            spd <- parseSpeed <$> httpGetStrTags (ssachPage b 0)
            liftIO $ putStrLn $ show spd
            return (b, spd)
        let (got, failed) = partition (isJust . snd) maybeb
            sorted = reverse $ sortBy (compare `on` fromJust . snd) got
        return (map (second fromJust) sorted, fst $ unzip $ failed)
