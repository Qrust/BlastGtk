module BlastItWithPiss
    (PerWipe(..)
    ,PerBoard(..)
    ,CaptchaType(..)
    ,CaptchaAnswer(..)
    ,OriginStamp(..)
    ,renderCompactStamp
    ,Message(..)
    ,OutMessage(..)
    ,LogDetail(..)
    ,PerProxy(..)
    ,entryPoint
    ,sortSsachBoardsByPopularity
    ) where
import Import
import BlastItWithPiss.Blast
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.MultipartFormData
import BlastItWithPiss.Post
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Text.Show as Show
import Text.HTML.TagSoup(Tag)
import Data.Time.Clock.POSIX

{-
import Control.Concurrent (forkIO)
import GHC.Conc (threadStatus, ThreadStatus(..))
import System.Process
import System.Exit
import Network
import qualified Data.ByteString.Lazy as L
--}

-- TODO move to where, shouldn't be top-level
{-# INLINE maybeSTM #-}
maybeSTM :: (Functor m, MonadIO m) => TVar (Maybe a) -> (a -> b) -> m b -> m b
maybeSTM t d m = maybe m (return . d) =<< liftIO (readTVarIO t)

{-# INLINE flMaybeSTM #-}
flMaybeSTM :: MonadIO m => TVar (Maybe a) -> (a -> m b) -> m b -> m b
flMaybeSTM t d m = maybe m d =<< liftIO (readTVarIO t)

{-# INLINE flBoolModSTM #-}
flBoolModSTM :: MonadIO m => TVar Bool -> (a -> m a) -> a -> m a
flBoolModSTM t f v = ifM (liftIO $ readTVarIO t) (f v) (return v)

-- HORRIBLE
-- HACK abortWithOutcome
data AbortOutcome = AbortOutcome !Outcome
    deriving (Show, Typeable)

instance Exception AbortOutcome

abortWithOutcome :: Outcome -> Blast a
abortWithOutcome o = do
    log $ "Aborting with outcome: " ++ show o
    sendOut $ OutcomeMessage o
    throwIO (AbortOutcome o)
-- /HACK

blastPostData :: Mode -> (Int -> Blast ParsedThread) -> Maybe Page -> Maybe Int -> Blast PostData
blastPostData mode getThread mpastapage thread = do
    PerWipe{..} <- asks bPerWipe
    log "Choosing pasta..."
    (nopastas, ((escinv, escwrd), pasta)) <- do
        pastagen <- liftIO $ readTVarIO tpastagen
        pastagen getThread mpastapage thread
    when nopastas $ do
        sendOut NoPastas
        log "threw NoPastas"
    log $ "chose pasta, escaping invisibles " ++ show escinv ++
        ", escaping wordfilter " ++ show escwrd ++ ": \"" ++ pasta ++ "\""
    log "Choosing image"
    (noimages, cleanImage) <- do
        use <- liftIO $ readTVarIO tuseimages
        if not use && not (obligatoryImageMode mode) || obligatoryNoImageMode mode
            then return (False, Nothing)
            else second Just <$> (liftIO . ($ use) =<< liftIO (readTVarIO timagegen))
    when noimages $ do
        sendOut NoImages
        log "threw NoImages"
    junkImage <- case cleanImage of
        Nothing -> do
            log "chose no image"
            return Nothing
        Just i -> do
            log $ "chose image \"" ++ filename i ++ "\""
            Just <$> flBoolModSTM tappendjunkimages
                (\im -> do log "appending junk to image"
                           appendJunk im) i
    watermark <- liftIO $ readTVarIO tmakewatermark
    log $ "Watermark: " ++ show watermark
    let final = PostData "" pasta junkImage (sageMode mode) watermark escinv escwrd
    final `deepseq` return final

blastCaptcha :: String -> Maybe Int -> Blast (Bool, String, Maybe (String, (OriginStamp -> IO ())))
blastCaptcha wakabapl thread = do
    board <- asks bBoard
    chKey <- getChallengeKey ssachRecaptchaKey
    log "Downloading captcha"
    mbbytes <- ssachGetCaptcha board thread ssachRecaptchaKey chKey
    case mbbytes of
        Nothing -> do
            log $ "Couldn't download captcha " ++ show chKey
            return (False, chKey, Just ("", const $ return ()))
        Just bytes -> do
            log "Got captcha image, sending captcha mvar"
            m <- newEmptyMVar
            sendOut $ SupplyCaptcha CaptchaPosting bytes (putMVar m $!!)
            log "blocking on captcha mvar"
            a <- takeMVar m
            log $ "got captcha mvar, answer is... " ++ show a
            case a of
                Answer s r -> return (True, chKey, Just (s, r))
                ReloadCaptcha -> blastCaptcha wakabapl thread
                AbortCaptcha -> return (True, chKey, Nothing)

-- TODO Should be buggy as hell.
-- FIXME Code duplication with "post"
blastCloudflare :: (Response [Tag Text] -> Blast b) -> Blast (Response [Tag Text]) -> String -> Blast b
blastCloudflare md whatrsp url = do
    log "Starting blastCloudflare"
    blastCloudflare' =<< whatrsp
  where blastCloudflare' rsp
            | responseStatus rsp == status404 && (maybe False (=="NWS_QPLUS_HY") $
                lookup hServer $ responseHeaders rsp) = do
                abortWithOutcome Four'o'FourBan -- HACK HACK HACK oyoyoyoy
            | responseStatus rsp == status403 && cloudflareBan (responseBody rsp) = do
                abortWithOutcome CloudflareBan -- HACK HACK HACK oyoyoyoy
            | responseStatus rsp == status403 && cloudflareCaptcha (responseBody rsp) =
                cloudflareChallenge
            | otherwise = do
                log "blastCloudflare: No cloudflare spotted..."
                md rsp
        cloudflareChallenge = do
            log "Encountered cloudflare challenge"
            PerProxy{..} <- asks bPerProxy
            (empt, work) <- liftIO $ atomically $ do
                empt <- isEmptyTMVar psharedCookies
                work <- isEmptyTMVar pcloudflareCaptchaLock
                when (empt && not work) $
                    takeTMVar pcloudflareCaptchaLock
                return (empt, work)
            if not empt || work
                then do
                    log "Waiting for cloudflare cookies..."
                    void $ liftIO $ atomically $ readTMVar pcloudflareCaptchaLock
                    nothingyet <- liftIO $ atomically $ isEmptyTMVar psharedCookies
                    if nothingyet
                        then cloudflareChallenge
                        else do log "Got cloudflare cookies"
                                runHttp $ setCookieJar =<< liftIO (atomically $ readTMVar psharedCookies)
                                md =<< whatrsp
                else handle (\(a::SomeException) -> do
                                liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                                throwIO a) $ do
                    log "locked cloudflare captcha"
                    chKey <- getChallengeKey cloudflareRecaptchaKey
                    bytes <- getCaptchaImage chKey
                    m <- newEmptyMVar
                    -- FIXME wait, why the hell don't we use blastCaptcha?
                    -- FIXME HACK HORRIBLE What the fuck is this shit?
                    sendOut $ SupplyCaptcha CaptchaCloudflare bytes (putMVar m $!!)
                    a <- takeMVar m
                    case a of
                        Answer s _ -> do
                            let rq = urlEncodedBody
                                    [("recaptcha_challenge_key", fromString chKey)
                                    ,("recaptcha_response_key", fromString s)
                                    ,("message", "")
                                    ,("act", "captcha")
                                    ] $ (fromJust $ parseUrl url)
                                        {checkStatus = \_ _ -> Nothing
                                        ,redirectCount = 0}
                            void $ httpReq rq
                            ck <- runHttp $ getCookieJar
                            let ckl = length $ destroyCookieJar ck
                            if ckl==0
                                then do log "Couldn't get Cloudflare cookies. Retrying."
                                        liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                                        blastCloudflare md whatrsp url
                                else do log $ "Cloudflare cookie count: " ++ show (length $ destroyCookieJar ck)
                                        liftIO $ atomically $ do
                                            putTMVar pcloudflareCaptchaLock ()
                                            putTMVar psharedCookies ck
                                        log "finished working on captcha"
                                        md =<< whatrsp
                        ReloadCaptcha -> cloudflareChallenge
                        AbortCaptcha -> do
                            log "Aborting cloudflare captcha. This might have unforeseen consequences."
                            liftIO $ atomically $ do
                                putTMVar pcloudflareCaptchaLock ()
                            md =<< whatrsp

blastPost :: POSIXTime -> Bool -> POSIXTime -> POSIXTime -> (String, [Field]) -> Mode -> Maybe Int -> PostData -> Blast (POSIXTime, POSIXTime)
blastPost threadtimeout cap lthreadtime lposttime w@(wakabapl, otherfields) mode thread postdata = do
    BlastData{bBoard=board, bPerWipe=PerWipe{..}, bPerBoard=PerBoard{..}} <- ask
    (neededcaptcha, chKey, mcap) <-
        if cap || mode==CreateNew || not ssachAdaptivity
            then do
                log "querying captcha"
                blastCaptcha wakabapl thread
            else do
                log "skipping captcha"
                return (False, "", Just ("", const $ return ()))
    case mcap of
        Nothing -> return (lthreadtime, lposttime)
        Just (captcha, reportbad) -> do
            p <- prepare board thread postdata chKey captcha wakabapl
                                 otherfields ssachLengthLimit
            posttimeout <- maybeSTM mposttimeout realToFrac $
                            maybeSTM tposttimeout realToFrac $
                                return $ ssachPostTimeout board
            log $ "Post timeout: " ++ show posttimeout
            log $ "Thread timeout: " ++ show threadtimeout
            mfluctuation <- liftIO $ readTVarIO tfluctuation
            log $ "Post time fluctuation: " ++ show mfluctuation
            beforeSleep <- liftIO getPOSIXTime
            let canPost = beforeSleep - lposttime >= posttimeout
            when (mode /= CreateNew && not canPost) $ do
                fluctuation <- flip (maybe $ return 0) mfluctuation $ \f -> do
                    if neededcaptcha
                        then do
                            log "Needed to manually input captcha, cancelling fluctuation"
                            return 0
                        else do
                            lowerHalf <- chooseFromList [True, True, True, False]
                            r <- realToFrac <$> getRandomR
                                (if lowerHalf then 0 else f/2, if lowerHalf then f/2 else f)
                            log $ "Sleep added by fluctuation: " ++ show r
                            return r
                let slptime = (lposttime + posttimeout) - beforeSleep + fluctuation
                log $ "sleeping " ++ show slptime ++ " seconds before post. FIXME using threadDelay for sleeping, instead of a more precise timer"
                liftIO $ threadDelay $ round $ slptime * 1000000
            log "posting"
            beforePost <- liftIO $ getPOSIXTime --optimistic
            (out, _) <- post p
            afterPost <- liftIO $ getPOSIXTime --pessimistic
            sendOut (OutcomeMessage out)
            when (successOutcome out) $ log "post succeded"
            let (nthreadtime, nposttime) =
                    if mode == CreateNew
                        then (afterPost, lposttime) --pessimistic
                        else (lthreadtime, beforePost) --optimistic
            case out of
                Success -> return (nthreadtime, nposttime)
                SuccessLongPost rest ->
                    if mode /= CreateNew
                        then blastPost threadtimeout cap nthreadtime nposttime w mode thread
                                (PostData "" rest Nothing (sageMode mode) False (escapeInv postdata) (escapeWrd postdata))
                        else return (nthreadtime, nposttime)
                TooFastPost -> do
                    log "TooFastPost, retrying in 0.5 seconds"
                    return (lthreadtime, beforePost - (posttimeout - 0.5))
                TooFastThread -> do
                    let m = threadtimeout / 2
                    log $ "TooFastThread, retrying in " ++ show (m/60) ++ " minutes"
                    return (beforePost - m, lposttime)
                PostRejected -> do
                    log "PostRejected, retrying later..."
                    return (lthreadtime, lposttime)
                o | o==NeedCaptcha || o==WrongCaptcha -> do
                    log $ show o ++ ", requerying"
                    liftIO . reportbad =<< genOriginStamp
                    blastPost threadtimeout True lthreadtime lposttime w mode thread postdata
                  | otherwise -> do
                    log "post failed, retrying in 0.5 seconds"
                    return (lthreadtime, beforePost - (posttimeout - 0.5))

blastLoop :: (String, [Field]) -> POSIXTime -> POSIXTime -> Blast ()
blastLoop w lthreadtime lposttime = do
    BlastData{bBoard=board, bPerWipe=PerWipe{..}, bPerBoard=PerBoard{..}} <- ask
    let getPage p = do
            let url = ssachPage board p
            let chkStatus _ _ = Nothing
            log $ "getPage: going to page " ++ show p
            blastCloudflare (return . parsePage board . responseBody)
                (httpReqStrTags $
                    (fromJust $ parseUrl url){checkStatus=chkStatus}) url
    let getThread i = do
            log $ "Going into " ++ show i ++ " thread for pasta"
            headNote "PastaHead fail" . fst . parseThreads <$> httpGetStrTags (ssachThread board (Just i))
    now <- liftIO $ getPOSIXTime
    threadtimeout <- maybeSTM mthreadtimeout realToFrac $
                        maybeSTM tthreadtimeout realToFrac $
                            return $ ssachThreadTimeout board
    canmakethread <- ifM (liftIO $ readTVarIO tcreatethreads)
                        (return $ now - lthreadtime >= threadtimeout)
                        (return False)
    mp0 <- flMaybeSTM mthread (const $ return Nothing) $ do
                log "mp0: Getting first page."
                Just <$> getPage 0
    flip (maybe $ log "Thread chosen, ommitting page parsing") mp0 $ \p0 ->
        log $ "page params:\n" ++
                   "page id: " ++ show (pageId p0) ++ "\n" ++
                   "lastpage id: " ++ show (lastpage p0) ++ "\n" ++
                   "speed: " ++ show (speed p0) ++ "\n" ++
                   "threads: " ++ show (length $ threads p0) ++ "\n" ++
                   "max replies: " ++ maybe "COULDN'T PARSE THREADS, EXPECT CRASH IN 1,2,3..." show (maximumMay $ map postcount $ threads p0)
    mode <- flMaybeSTM mmode (\m -> do log $ "Got mmode " ++ show m; return m) $ 
        maybe (do log "No page, throwing a dice for SagePopular/BumpUnpopular"
                  chooseFromList [SagePopular, BumpUnpopular])
              (\p0 -> do log "Choosing mode..."
                         chooseMode board canmakethread p0) mp0
    recMode mode
    log $ "chose mode " ++ show mode
    (thread, mpastapage) <- flMaybeSTM mthread
        (\t -> do log $ "Got mthread " ++ show t
                  return (Just t, Nothing)) $ do
        log "Choosing thread..."
        second Just <$> chooseThread board mode getPage
            (fromMaybe (error "Page is Nothing while thread specified") mp0)
    recThread thread
    log $ "chose thread " ++ show thread
    postdata <- blastPostData mode getThread mpastapage thread
    (nthreadtime, nposttime) <- blastPost threadtimeout False lthreadtime lposttime w mode thread postdata
    blastLoop w nthreadtime nposttime

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
entryPoint :: Proxy -> Board -> LogDetail -> PerWipe -> PerBoard -> PerProxy -> (OutMessage -> IO ()) -> Blast ()
entryPoint proxy board lgDetail shS muS prS output = do
    runBlast (BlastData proxy board lgDetail shS muS prS output) $ do
        log "Entry point"
        httpSetProxy proxy
        let hands =
              [Handler $ \(a::AsyncException) -> do
                log $ "Got async " ++ show a
                throwIO a
              {-,Handler $ \(a::HttpException) -> do
                log $ "Got http exception, restarting. Exception was: " ++ show a
                start -- Dunno what to do except restart.-}
              ,Handler $ \(_::AbortOutcome) -> start
                -- HACK abortOutcome
                -- ABSOLUTELY DISGUSTING HACK: mainloop will kill this thread when it receives one of CloudflareBan, Four'o'FourBan.
              ,Handler $ \(a::SomeException) -> do
                log $ "Terminated by exception " ++ show a
                sendOut $ OutcomeMessage $ InternalError $ ErrorException a
              ]
            start = flip catches hands $
                blastLoop (ssachLastRecordedWakabaplAndFields board) 0 0
        start{-
        let url = ssachPage board 0
        let chkStatus st@Status{statusCode=c} heads
                | c /= 200 && c /= 403 = Just $ toException $ StatusCodeException st heads Nothing
                | otherwise = Nothing
        x <- try $ do
            log $ "Downloading page form"
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
                log $ "Couldn't parse page form, got exception " ++ show a
            Right w -> do
                log "Starting loop"
                --log $ show $ length $ (show w :: String)
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
