-- | Kludge
module BlastItWithPiss
    (ShSettings(..)
    ,MuSettings(..)
    ,CaptchaType(..)
    ,CaptchaAnswer(..)
    ,OriginStamp(..)
    ,renderCompactStamp
    ,SupplyCaptcha(..)
    ,Message(..)
    ,OutMessage(..)
    ,LogDetail(..)
    ,ProxySettings(..)
    ,defMuS
    ,defPrS
    ,entryPoint
    ,sortSsachBoardsByPopularity

    -- FIXME
    ,TempBlastCaptchaChannel(..)
    ) where
import Import

import BlastItWithPiss.Blast
import BlastItWithPiss.ImageGen
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Post

import Text.Recognition.Antigate

import Control.Concurrent.Lifted
import Control.Concurrent.STM

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Resource

import Data.Time.Clock.POSIX

import qualified Text.Show as Show
import Text.HTML.TagSoup(Tag)

data ShSettings = ShSettings
    {tpastagen :: TVar ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO TempBlastCaptchaChannel)
    ,timagegen :: TVar (IO Image)
    --NOTE all browser state accumulated in gens is lost.
    ,tuseimages :: TVar Bool
    ,tappendjunkimages :: TVar Bool
    ,tcreatethreads :: TVar Bool
    ,tmakewatermark :: TVar Bool
    ,tposttimeout :: TVar (Maybe Double)
    ,tthreadtimeout :: TVar (Maybe Double)
    ,tfluctuation :: TVar (Maybe Double)
    ,tsage :: TVar Bool
    }

data MuSettings = MuSettings
    {mthread :: TVar (Maybe Int)
    ,mmode :: TVar (Maybe Mode)
    ,mposttimeout :: TVar (Maybe Double)
    ,mthreadtimeout :: TVar (Maybe Double)
    }

data CaptchaType
    = CaptchaPosting
    | CaptchaCloudflare

data CaptchaAnswer
    = Answer !String !(OriginStamp -> IO ())
    | ReloadCaptcha
    | AbortCaptcha

data OriginStamp = OriginStamp
    {oTime :: !ZonedTime
    ,oProxy :: !BlastProxy
    ,oBoard :: !Board
    ,oMode :: !Mode
    ,oThread :: !(Maybe Int)
    }

data SupplyCaptcha = SupplyCaptcha
    {captchaType :: !CaptchaType
    ,captchaBytes :: !LByteString
    ,captchaSend :: !(CaptchaAnswer -> IO ())
    ,captchaConf :: !CaptchaConf
    ,captchaFilename :: !String
    }

data Message
    = OutcomeMessage !Outcome
    | LogMessage !String
    | SolveCaptcha !SupplyCaptcha
    | NoPastas
    | NoImages

data OutMessage = OutMessage !OriginStamp !Message

data LogDetail
    = Log
    | Don'tLog
  deriving (Eq, Show, Ord, Enum, Bounded)

data ProxySettings = ProxyS
    {psharedCookies :: !(TMVar CookieJar)
    ,pcloudflareCaptchaLock :: !(TMVar ())
    }

data BlastLogData = BlastLogData
    {bldProxy :: !BlastProxy
    ,bldBoard :: !Board
    ,bldLogD :: !LogDetail
    ,bldShS :: !ShSettings
    ,bldMuS :: !MuSettings
    ,bldPrS :: !ProxySettings
    ,bldOut :: !(OutMessage -> IO ())
    ,bldOtherFields :: ![Part Blast (ResourceT IO)] -- ^ Kludge
    }

data OriginInfo = OriginInfo
    {gmode :: !Mode
    ,gthread :: !(Maybe Int)
    }

data BlastState = BlastState
    {bsOriginInfo :: !OriginInfo
    ,bsAdaptivityIn :: !Bool
    ,bsOptimisticLastPostTime :: !POSIXTime
    ,bsPessimisticLastPostTime :: !POSIXTime
    ,bsLastThreadTime :: !POSIXTime -- ^ Always pessimistic
    }

instance Default BlastState where
    def = BlastState def False 0 0 0

type BlastLog = ReaderT BlastLogData (StateT BlastState Blast)

instance Show CaptchaAnswer where
    show (Answer a _) = "Answer " ++ show a ++ " <repBad>"
    show ReloadCaptcha = "ReloadCaptcha"
    show AbortCaptcha = "AbortCaptcha"

renderCompactStamp :: OriginStamp -> String
renderCompactStamp (OriginStamp _ proxy board _ _) =
    renderBoard board ++ " {" ++ show proxy ++ "}"

instance Show OriginStamp where
    show (OriginStamp time proxy board mode thread) =
        "(" ++ show time ++ ") " ++ "{" ++ show proxy ++ "} " ++ renderBoard board ++
        " " ++ show mode ++ " [| " ++
        ssachThread board thread ++ " |]"

instance Show Message where
    show (OutcomeMessage o) = show o
    show (LogMessage o) = o
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

instance NFData CaptchaConf

instance NFData SupplyCaptcha where
    rnf (SupplyCaptcha a b c d e) = rnf(a,b,c,d,e)

instance NFData Message where
    rnf (OutcomeMessage o) = rnf o
    rnf (LogMessage s) = rnf s
    rnf (SolveCaptcha s) = rnf s
    rnf _ = ()

instance NFData OutMessage where
    rnf (OutMessage os m) = os `deepseq` m `deepseq` ()

{-# INLINE maybeSTM #-}
maybeSTM :: (Functor m, MonadIO m) => TVar (Maybe a) -> (a -> b) -> m b -> m b
maybeSTM t d m = maybe m (return . d) =<< liftIO (readTVarIO t)

{-# INLINE flMaybeSTM #-}
flMaybeSTM :: MonadIO m => TVar (Maybe a) -> (a -> m b) -> m b -> m b
flMaybeSTM t d m = maybe m d =<< liftIO (readTVarIO t)

{-# INLINE flBoolModSTM #-}
flBoolModSTM :: MonadIO m => TVar Bool -> (a -> m a) -> a -> m a
flBoolModSTM t f v = ifM (liftIO $ readTVarIO t) (f v) (return v)

defMuS :: IO MuSettings
defMuS = atomically $
    MuSettings <$> newTVar Nothing <*> newTVar Nothing <*> newTVar Nothing <*> newTVar Nothing

defPrS :: IO ProxySettings
defPrS = atomically $ do
    psharedCookies <- newEmptyTMVar
    pcloudflareCaptchaLock <- newTMVar ()
    return ProxyS{..}

runBlastLog :: BlastLogData -> BlastLog a -> Blast a
runBlastLog d m = evalStateT (runReaderT m d) def

runBlastLogSt :: BlastLogData -> BlastState -> BlastLog a -> Blast a
runBlastLogSt d o m = evalStateT (runReaderT m d) o

blast :: Blast a -> BlastLog a
blast = lift . lift

askProxy :: BlastLog BlastProxy
askProxy = asks bldProxy

askBoard :: BlastLog Board
askBoard = asks bldBoard

askLogD :: BlastLog LogDetail
askLogD = asks bldLogD

askOrI :: BlastLog OriginInfo
askOrI = bsOriginInfo <$> lift get

askShS :: BlastLog ShSettings
askShS = asks bldShS

askBSM :: BlastLog (Board, ShSettings, MuSettings)
askBSM = asks $ \b -> (bldBoard b, bldShS b, bldMuS b)

askAdaptive :: BlastLog Bool
askAdaptive = bsAdaptivityIn <$> lift get

setAdaptive :: Bool -> BlastLog ()
setAdaptive n = lift $ get >>= \s -> put s{bsAdaptivityIn=n}

askProxyS :: BlastLog ProxySettings
askProxyS = asks bldPrS

askOut :: BlastLog (OutMessage -> IO ())
askOut = asks bldOut

recMode :: Mode -> BlastLog ()
recMode m = lift get >>= \s@BlastState{..} -> lift $ put s{bsOriginInfo=bsOriginInfo{gmode=m}}

recThread :: (Maybe Int) -> BlastLog ()
recThread t = lift get >>= \s@BlastState{..} -> lift $ put s{bsOriginInfo=bsOriginInfo{gthread=t}}

setLastThreadTime :: POSIXTime -> BlastLog ()
setLastThreadTime t = lift get >>= \s -> lift $ put s{bsLastThreadTime=t}

setOptimisticLastPostTime :: POSIXTime -> BlastLog ()
setOptimisticLastPostTime t = lift get >>= \s -> lift $ put s{bsOptimisticLastPostTime=t}

setPessimisticLastPostTime :: POSIXTime -> BlastLog ()
setPessimisticLastPostTime t = lift get >>= \s -> lift $ put s{bsPessimisticLastPostTime=t}

genOriginStamp :: BlastLog OriginStamp
genOriginStamp = do
    proxy <- askProxy
    board <- askBoard
    OriginInfo{..} <- askOrI
    now <- liftIO getZonedTime
    return $ OriginStamp now proxy board gmode gthread

blastOut :: Message -> BlastLog ()
blastOut msg = do
    to <- askOut
    st <- genOriginStamp
    let a = OutMessage st msg
    liftIO $ a `deepseq` to a

blastLog :: String -> BlastLog ()
blastLog msg = do
    d <- askLogD
    when (d == Log) $ do
        blastOut (LogMessage msg)

-- HACK abortWithOutcome
data AbortOutcome = AbortOutcome !Outcome
    deriving (Show, Typeable)

instance Exception AbortOutcome

abortWithOutcome :: Outcome -> BlastLog a
abortWithOutcome o = do
    blastLog $ "Aborting with outcome: " ++ show o
    blastOut $ OutcomeMessage o
    throwIO (AbortOutcome o)
-- /HACK

data TempBlastCaptchaChannel =
    TBCC{nopastas :: Bool
        ,escinv :: Bool
        ,escwrd :: Bool
        ,pasta :: String} 

blastPostData :: Mode -> (Int -> BlastLog Thread) -> Maybe Page -> Maybe Int -> BlastLog PostData
blastPostData mode getThread mpastapage thread = do
    ShSettings{..} <- askShS
    blastLog "Choosing pasta..."
    TBCC{..} <- do
        pastagen <- liftIO $ readTVarIO tpastagen
        r <- ask
        s <- lift get
        st <- blast getBrowserState
        manager <- blast getManager
        liftIO $ pastagen
            (runBlast manager st . runBlastLogSt r s . getThread)
                    mpastapage thread
    {-when nopastas $ do
        blastOut NoPastas
        blastLog "threw NoPastas"-}
    blastLog $ "chose pasta, escaping invisibles " ++ show escinv ++
        ", escaping wordfilter " ++ show escwrd ++ ": \"" ++ pasta ++ "\""
    blastLog "Choosing image"
    cleanImage <- do
        use <- liftIO $ readTVarIO tuseimages
        if (not use && not (obligatoryImageMode mode) || obligatoryNoImageMode mode)
           && not nopastas
          then
            return Nothing
          else do
            imagegen <- liftIO $ readTVarIO timagegen
            raw <- liftIO imagegen
            return $ Just raw
    junkImage <-
        case cleanImage of
          Nothing -> do
            blastLog "chose no image"
            return Nothing
          Just i -> do
            blastLog $ "chose image \"" ++ filename i ++ "\""
            Just <$> flBoolModSTM tappendjunkimages
                (\im -> do blastLog "appending junk to image"
                           appendJunk im) i
    watermark <- liftIO $ readTVarIO tmakewatermark
    blastLog $ "Watermark: " ++ show watermark
    let final = PostData "" pasta junkImage (sageMode mode) watermark escinv escwrd
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
        Right chKey -> do
            let _ = chKey `asTypeOf` currentSsachCaptchaType
            blastLog "Downloadng captcha"
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

-- TODO Should be buggy as hell.
-- FIXME Code duplication with "post"
blastCloudflare :: (Response [Tag Text] -> BlastLog b) -> BlastLog (Response [Tag Text]) -> String -> BlastLog b
blastCloudflare md whatrsp url = do
    blastLog "Starting blastCloudflare"
    blastCloudflare' =<< whatrsp
  where
    blastCloudflare' rsp
        | responseStatus rsp == status404 && (maybe False (=="NWS_QPLUS_HY") $
            lookup hServer $ responseHeaders rsp) = do
            abortWithOutcome Four'o'FourBan -- HACK HACK HACK oyoyoyoy
        | responseStatus rsp == status403 && cloudflareBan (responseBody rsp) = do
            abortWithOutcome CloudflareBan -- HACK HACK HACK oyoyoyoy
        | responseStatus rsp == status403 && cloudflareCaptcha (responseBody rsp) =
            cloudflareChallenge
        | otherwise = do
            blastLog "blastCloudflare: No cloudflare spotted..."
            md rsp
    cloudflareChallenge = do
        blastLog "Encountered cloudflare challenge"

        ProxyS{..} <- askProxyS

        (nothingyet, jobtaken) <- liftIO $ atomically $ do
            nothingyet <- isEmptyTMVar psharedCookies
            jobtaken <- isEmptyTMVar pcloudflareCaptchaLock
            when (nothingyet && not jobtaken) $
                takeTMVar pcloudflareCaptchaLock
            return (nothingyet, jobtaken)
        if not nothingyet || jobtaken
          then do
            blastLog "Waiting for cloudflare cookies..."

            void $ liftIO $ atomically $ readTMVar pcloudflareCaptchaLock
            stillnothing <- liftIO $ atomically $ isEmptyTMVar psharedCookies

            if stillnothing
              then cloudflareChallenge
              else do
                blastLog "Got cloudflare cookies"
                blast $ setCookieJar =<< liftIO (atomically $ readTMVar psharedCookies)
                md =<< whatrsp
          else (do
            -- OH GOD
            blastLog "locked cloudflare captcha"

            chKey <- blast $ recaptchaChallengeKey cloudflareRecaptchaKey
            (bytes, ct) <- blast $ getCaptchaImage $ Recaptcha chKey
            cconf <- blast $ getCaptchaConf $ Recaptcha chKey

            fname <- mkImageFileName ct
            m <- newEmptyMVar
            blastOut $ SolveCaptcha $ SupplyCaptcha
                    {captchaType = CaptchaCloudflare
                    ,captchaBytes = bytes
                    ,captchaSend = \a -> a `deepseq` putMVar m a
                    ,captchaConf = cconf
                    ,captchaFilename = fname
                    }
            a <- takeMVar m
            case a of
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

                void $ blast $ httpReq rq
                ck <- blast $ getCookieJar

                let cookieCount = length $ destroyCookieJar ck
                if cookieCount == 0
                  then do
                    blastLog "Couldn't get Cloudflare cookies. Retrying."

                    liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                    blastCloudflare md whatrsp url
                  else do
                    blastLog $ "Cloudflare cookie count: " ++
                               show cookieCount

                    liftIO $ atomically $ do
                        putTMVar pcloudflareCaptchaLock ()
                        putTMVar psharedCookies ck

                    blastLog "finished working on captcha"
                    md =<< whatrsp
              ReloadCaptcha ->
                cloudflareChallenge
              AbortCaptcha -> do
                blastLog "Aborting cloudflare captcha. ???????? consequences."

                liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                md =<< whatrsp
            ) `catch` (\(a::SomeException) -> do
                liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                throwIO a)

blastPost :: POSIXTime -> Bool -> [Part Blast (ResourceT IO)] -> Mode -> Maybe Int -> PostData -> BlastLog ()
blastPost threadtimeout cap otherfields mode thread postdata = do
    (board, ShSettings{..}, MuSettings{..}) <- askBSM
    BlastState{..} <- lift get
    adaptive <- askAdaptive
    (neededcaptcha, mcap) <-
        if cap || mode==CreateNew || not adaptive
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

            posttimeout <-
                maybeSTM mposttimeout realToFrac $
                    maybeSTM tposttimeout realToFrac $
                        return $ ssachPostTimeout board
            blastLog $ "Post timeout: " ++ show posttimeout
            blastLog $ "Thread timeout: " ++ show threadtimeout

            mfluctuation <- liftIO $ readTVarIO tfluctuation
            blastLog $ "Post time fluctuation: " ++ show mfluctuation

            beforeSleep <- liftIO getPOSIXTime
            let canDefinitelyPost =
                    beforeSleep - bsPessimisticLastPostTime >= posttimeout

            when (mode /= CreateNew && not canDefinitelyPost) $ do
                fluctuation <- flip (maybe $ return 0) mfluctuation $ \f -> do
                    lowerHalf <- chooseFromList [True, True, True, False]
                    r <- realToFrac <$> getRandomR
                        (if lowerHalf then 0 else f/2, if lowerHalf then f/2 else f)
                    blastLog $ "Sleep added by fluctuation: " ++ show r
                    return r

                let slptime =
                        (bsOptimisticLastPostTime + posttimeout)
                        - beforeSleep + fluctuation

                blastLog $ "sleeping " ++ show slptime ++
                    " seconds before post. FIXME using threadDelay for sleeping"
                    ++ ", instead of a more precise timer"
                liftIO $ threadDelay $ round $ slptime * 1000000

            blastLog "posting"

            beforePost <- liftIO $ getPOSIXTime --optimistic
            (out, _) <- blast $ post p
            afterPost <- liftIO $ getPOSIXTime --pessimistic

            blastOut (OutcomeMessage out)

            when (successOutcome out) $ do
                if not neededcaptcha && not adaptive
                    then do
                        setAdaptive True
                    else do
                        when (neededcaptcha && adaptive) $ setAdaptive False
                if mode==CreateNew
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
                        blastPost threadtimeout cap otherfields mode thread $ PostData
                            {subject = subject postdata
                            ,text = rest
                            ,image = Nothing
                            ,sage = sageMode mode
                            ,makewatermark = makewatermark postdata
                            ,escapeInv = escapeInv postdata
                            ,escapeWrd = escapeWrd postdata}
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
                        then blastLog "TooFastPost, retrying in 0.5 seconds"
                        else blastLog "post failed, retrying in 0.5 seconds"
                    setOptimisticLastPostTime $ beforePost - (posttimeout - 0.5)
                    setPessimisticLastPostTime $ beforePost - (posttimeout - 0.5)

blastLoop :: BlastLog ()
blastLoop = forever $ do
    bldOtherFields <- asks bldOtherFields
    (board, ShSettings{..}, MuSettings{..}) <- askBSM
    BlastState{..} <- lift get
    now <- liftIO $ getPOSIXTime
    threadtimeout <- maybeSTM mthreadtimeout realToFrac $
                        maybeSTM tthreadtimeout realToFrac $
                            return $ ssachThreadTimeout board
    canmakethread <- ifM (liftIO $ readTVarIO tcreatethreads)
                        (return $ now - bsLastThreadTime >= threadtimeout)
                        (return False)
    mp0 <- flMaybeSTM mthread (const $ return Nothing) $ do
                blastLog "mp0: Getting first page."
                Just <$> getPage board 0
    flip (maybe $ blastLog "Thread chosen, ommitting page parsing") mp0 $ \p0 ->
        blastLog $ "page params:\n" ++
                   "page id: " ++ show (pageId p0) ++ "\n" ++
                   "lastpage id: " ++ show (lastpage p0) ++ "\n" ++
                   "speed: " ++ show (speed p0) ++ "\n" ++
                   "threads: " ++ show (length $ threads p0) ++ "\n" ++
                   "max replies: " ++ maybe "COULDN'T PARSE THREADS, EXPECT CRASH IN 1,2,3..." show (maximumMay $ map postcount $ threads p0)
    mode <- flMaybeSTM mmode (\m -> do
                                blastLog $ "Got mmode " ++ show m
                                return m) $ do
        case mp0 of
            Nothing -> do
                blastLog "No page, throwing a dice for SagePopular/BumpUnpopular"
                chooseFromList [SagePopular, BumpUnpopular]
            Just p0 -> do
                blastLog "Choosing mode..."
                sage' <- liftIO $ readTVarIO tsage
                chooseMode board canmakethread sage' p0
    recMode mode
    blastLog $ "chose mode " ++ show mode
    (thread, mpastapage) <- flMaybeSTM mthread
        (\t -> do blastLog $ "Got mthread " ++ show t
                  return (Just t, Nothing)) $ do
        blastLog "Choosing thread..."
        second Just <$> chooseThread board mode (getPage board)
            (fromMaybe (error "Page is Nothing while thread specified") mp0)
    recThread thread
    blastLog $ "chose thread " ++ show thread
    postdata <- blastPostData mode (getThread board) mpastapage thread
    blastPost threadtimeout False bldOtherFields mode thread postdata
  where
    getPage board p = do
        let url = ssachPage board p
        let chkStatus _ _ _ = Nothing
        blastLog $ "getPage: going to page " ++ show p
        blastCloudflare (return . parsePage board . responseBody)
            (blast $ httpReqStrTags $
                (fromJust $ parseUrl url){checkStatus=chkStatus})
            url

    getThread board i = do
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
entryPoint :: BlastProxy -> Board -> LogDetail -> ShSettings -> MuSettings -> ProxySettings -> (OutMessage -> IO ()) -> Blast ()
entryPoint proxy board lgDetail shS muS prS output = do
    runBlastLog (BlastLogData proxy board lgDetail shS muS prS output (ssachLastRecordedFields board)) $ do
        blastLog "Entry point"
        blast $ httpSetProxy proxy
        let hands =
              [Handler $ \(a::AsyncException) -> do
                blastLog $ "Got async " ++ show a
                throwIO a
              {-,Handler $ \(a::HttpException) -> do
                blastLog $ "Got http exception, restarting. Exception was: " ++ show a
                start -- Dunno what to do except restart.-}
              ,Handler $ \(_::AbortOutcome) -> do
                blastLog "HACK abortOutcome"
                start --HACK abortOutcome
              ,Handler $ \(a::SomeException) -> do
                blastLog $ "Terminated by exception " ++ show a
                blastOut $ OutcomeMessage $ InternalError $ ErrorException a
              ]
            start = flip catches hands $ blastLoop
        start{-
        let url = ssachPage board 0
        let chkStatus st@Status{statusCode=c} heads
                | c /= 200 && c /= 403 = Just $ toException $ StatusCodeException st heads Nothing
                | otherwise = Nothing
        x <- try $ do
            blastLog $ "Downloading page form"
            {-
            rsp <- blast $ httpReqStrTags (fromJust $ parseUrl url){checkStatus=chkStatus}
            parseForm ssach <$> blastCloudflare (blast $ httpGetStrTags url) url rsp
            -}
            {-
            blast $ lift . conduitParseForm ssach . responseBody =<< makeRequest (fromJust $ parseUrl url)
            -}
            return $ ssachLastRecordedWakabaplAndFields url
        case x of
            Left (a::SomeException) -> do
                blastLog $ "Couldn't parse page form, got exception " ++ show a
            Right w -> do
                blastLog "Starting loop"
                --blastLog $ show $ length $ (show w :: String)
                --forever (return () >> liftIO yield)
                blastLoop w 0 0-}

sortSsachBoardsByPopularity :: [Board] -> IO ([(Board, Int)], [Board])
sortSsachBoardsByPopularity boards = bracket (newManager def) closeManager $ flip runBlastNew $ do
    maybeb <- forM boards $ \b -> do
                liftIO $ putStr $ "Processing " ++ renderBoard b ++ ". Speed: "
                spd <- parseSpeed <$> httpGetStrTags (ssachPage b 0)
                liftIO $ putStrLn $ show spd
                return (b, spd)
    let (got, failed) = partition (isJust . snd) maybeb
        sorted = reverse $ sortBy (\(_,a) (_,b) -> compare (fromJust a) (fromJust b)) got
    return (map (second fromJust) sorted, fst $ unzip $ failed)
