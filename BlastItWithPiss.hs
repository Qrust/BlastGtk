module BlastItWithPiss
    (ShSettings(..)
    ,MuSettings(..)
    ,CaptchaType(..)
    ,CaptchaAnswer(..)
    ,OriginStamp(..)
    ,renderCompactStamp
    ,Message(..)
    ,OutMessage(..)
    ,LogDetail(..)
    ,ProxySettings(..)
    ,defMuS
    ,defPrS
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

data ShSettings = ShSettings {tpastagen :: TVar ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO ((Bool, ((Bool, Bool), String))))
                             ,timagegen :: TVar (Bool -> IO (Bool, Image))
                             ,tuseimages :: TVar Bool
                             ,tappendjunkimages :: TVar Bool
                             ,tcreatethreads :: TVar Bool
                             ,tmakewatermark :: TVar Bool
                             }

data MuSettings = MuSettings {mthread :: TVar (Maybe Int)
                             ,mmode :: TVar (Maybe Mode)
                             }

data CaptchaType = CaptchaPosting | CaptchaCloudflare

data CaptchaAnswer = Answer !String !(OriginStamp -> IO ())
                   | ReloadCaptcha
                   | AbortCaptcha

data OriginStamp = OriginStamp {oTime :: !ZonedTime
                               ,oProxy :: !BlastProxy
                               ,oBoard :: !Board
                               ,oMode :: !Mode
                               ,oThread :: !(Maybe Int)
                               }

data Message = OutcomeMessage !Outcome
             | LogMessage !String
             | SupplyCaptcha {captchaType :: !CaptchaType
                             ,captchaBytes :: !LByteString
                             ,captchaSend :: !(CaptchaAnswer -> IO ())
                             }
             | NoPastas
             | NoImages

data OutMessage = OutMessage !OriginStamp !Message

data LogDetail = Log
               | Don'tLog
    deriving (Eq, Show, Ord, Enum, Bounded)

data ProxySettings = ProxyS {psharedCookies :: !(TMVar CookieJar)
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
        }

data OriginInfo = OriginInfo {gmode :: !Mode, gthread :: !(Maybe Int)}

type BlastLog = ReaderT BlastLogData (StateT OriginInfo Blast)

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
    show SupplyCaptcha{} = "SupplyCaptcha"
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

instance NFData Message where
    rnf (OutcomeMessage o) = rnf o
    rnf (LogMessage s) = rnf s
    rnf (SupplyCaptcha c b s) = rnf (c, b) `deepseq` s `seq` ()
    rnf _ = ()

instance NFData OutMessage where
    rnf (OutMessage os m) = os `deepseq` m `deepseq` ()

instance MonadRandom m => MonadRandom (StateT s m) where
    getRandom = lift getRandom
    getRandoms = lift getRandoms
    getRandomR = lift . getRandomR
    getRandomRs = lift . getRandomRs

flMaybeSTM :: MonadIO m => TVar (Maybe a) -> (a -> m b) -> m b -> m b
flMaybeSTM t d m = maybe m d =<< liftIO (readTVarIO t)

flBoolModSTM :: MonadIO m => TVar Bool -> (a -> m a) -> a -> m a
flBoolModSTM t f v = ifM (liftIO $ readTVarIO t) (f v) (return v)

defMuS :: IO MuSettings
defMuS = atomically $ do
    mthread <- newTVar Nothing
    mmode <- newTVar Nothing
    return MuSettings{..}

defPrS :: IO ProxySettings
defPrS = atomically $ do
    psharedCookies <- newEmptyTMVar
    pcloudflareCaptchaLock <- newTMVar ()
    return ProxyS{..}

runBlastLog :: BlastLogData -> BlastLog a -> Blast a
runBlastLog d m = evalStateT (runReaderT m d) def

runBlastLogSt :: BlastLogData -> OriginInfo -> BlastLog a -> Blast a
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
askOrI = lift get

askShS :: BlastLog ShSettings
askShS = asks bldShS

askBSM :: BlastLog (Board, ShSettings, MuSettings)
askBSM = asks $ \b -> (bldBoard b, bldShS b, bldMuS b)

askProxyS :: BlastLog ProxySettings
askProxyS = asks bldPrS

askOut :: BlastLog (OutMessage -> IO ())
askOut = asks bldOut

recMode :: Mode -> BlastLog ()
recMode m = lift get >>= \s -> lift $ put s{gmode=m}

recThread :: (Maybe Int) -> BlastLog ()
recThread t = lift get >>= \s -> lift $ put s{gthread=t}

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

blastPostData :: Mode -> (Int -> BlastLog Thread) -> Maybe Page -> Maybe Int -> BlastLog PostData
blastPostData mode getThread mpastapage thread = do
    ShSettings{..} <- askShS
    blastLog "Choosing pasta..."
    (nopastas, ((escinv, escwrd), pasta)) <- do
        pastagen <- liftIO $ readTVarIO tpastagen
        r <- ask
        s <- lift get
        liftIO $ pastagen (runBlast . runBlastLogSt r s . getThread) mpastapage thread
    when nopastas $ do
        blastOut NoPastas
        blastLog "threw NoPastas"
    blastLog $ "chose pasta, escaping invisibles " ++ show escinv ++
        ", escaping wordfilter " ++ show escwrd ++ ": \"" ++ pasta ++ "\""
    blastLog "Choosing image"
    (noimages, cleanImage) <- do
        use <- liftIO $ readTVarIO tuseimages
        if not use && not (obligatoryImageMode mode) || obligatoryNoImageMode mode
            then return (False, Nothing)
            else second Just <$> (liftIO . ($ use) =<< liftIO (readTVarIO timagegen))
    when noimages $ do
        blastOut NoImages
        blastLog "threw NoImages"
    junkImage <- case cleanImage of
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
    return (PostData "" pasta junkImage (sageMode mode) watermark escinv escwrd)

blastCaptcha :: String -> Maybe Int -> BlastLog (String, Maybe (String, (OriginStamp -> IO ())))
blastCaptcha wakabapl thread = do
    board <- askBoard
    chKey <- blast $ getChallengeKey ssachRecaptchaKey
    blastLog "Downloading captcha"
    mbbytes <- blast $ ssachGetCaptcha board thread ssachRecaptchaKey chKey
    case mbbytes of
        Nothing -> do
            blastLog "Couldn't download captcha"
            return (chKey, Just ("", const $ return ()))
        Just bytes -> do
            blastLog "Got captcha image, sending captcha mvar"
            m <- newEmptyMVar
            blastOut $ SupplyCaptcha CaptchaPosting bytes (putMVar m $!!)
            blastLog "blocking on captcha mvar"
            a <- takeMVar m
            blastLog $ "got captcha mvar, answer is... " ++ show a
            case a of
                Answer s r -> return (chKey, Just (s, r))
                ReloadCaptcha -> blastCaptcha wakabapl thread
                AbortCaptcha -> return (chKey, Nothing)

-- TODO Should be buggy as hell.
-- FIXME Code duplication with "post"
blastCloudflare :: (Response [Tag Text] -> BlastLog b) -> BlastLog (Response [Tag Text]) -> String -> BlastLog b
blastCloudflare md whatrsp url = do
    blastLog "Starting blastCloudflare"
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
                blastLog "blastCloudflare: No cloudflare spotted..."
                md rsp
        cloudflareChallenge = do
            blastLog "Encountered cloudflare challenge"
            ProxyS{..} <- askProxyS
            (empt, work) <- liftIO $ atomically $ do
                empt <- isEmptyTMVar psharedCookies
                work <- isEmptyTMVar pcloudflareCaptchaLock
                when (empt && not work) $
                    takeTMVar pcloudflareCaptchaLock
                return (empt, work)
            if not empt || work
                then do
                    blastLog "Waiting for cloudflare cookies..."
                    void $ liftIO $ atomically $ readTMVar pcloudflareCaptchaLock
                    nothingyet <- liftIO $ atomically $ isEmptyTMVar psharedCookies
                    if nothingyet
                        then cloudflareChallenge
                        else do blastLog "Got cloudflare cookies"
                                blast $ setCookieJar =<< liftIO (atomically $ readTMVar psharedCookies)
                                md =<< whatrsp
                else handle (\(a::SomeException) -> do
                                liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                                throwIO a) $ do
                    blastLog "locked cloudflare captcha"
                    chKey <- blast $ getChallengeKey cloudflareRecaptchaKey
                    bytes <- blast $ getCaptchaImage chKey
                    m <- newEmptyMVar
                    -- FIXME wait, why the hell don't we use blastCaptcha?
                    blastOut $ SupplyCaptcha CaptchaCloudflare bytes (putMVar m $!!)
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
                            void $ blast $ httpReq rq
                            ck <- blast $ getCookieJar
                            let ckl = length $ destroyCookieJar ck
                            if ckl==0
                                then do blastLog "Couldn't get Cloudflare cookies. Retrying."
                                        liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                                        blastCloudflare md whatrsp url
                                else do blastLog $ "Cloudflare cookie count: " ++ show (length $ destroyCookieJar ck)
                                        liftIO $ atomically $ do
                                            putTMVar pcloudflareCaptchaLock ()
                                            putTMVar psharedCookies ck
                                        blastLog "finished working on captcha"
                                        md =<< whatrsp
                        ReloadCaptcha -> cloudflareChallenge
                        AbortCaptcha -> do
                            blastLog "Aborting cloudflare captcha. This might have unforeseen consequences."
                            liftIO $ atomically $ do
                                putTMVar pcloudflareCaptchaLock ()
                            md =<< whatrsp

blastPost :: Bool -> POSIXTime -> POSIXTime -> (String, [Field]) -> Mode -> Maybe Int -> PostData -> BlastLog (POSIXTime, POSIXTime)
blastPost cap lthreadtime lposttime w@(wakabapl, otherfields) mode thread postdata = do
    (board, ShSettings{..}, _) <- askBSM
    (chKey, mcap) <- if cap || mode==CreateNew || not ssachAdaptivity
                        then do blastLog "querying captcha"
                                blastCaptcha wakabapl thread
                        else do blastLog "skipping captcha"
                                return ("", Just ("", const $ return ()))
    case mcap of
        Nothing -> return (lthreadtime, lposttime)
        Just (captcha, reportbad) -> do
            -- TODO post reposts
            p <- blast $ prepare board thread postdata chKey captcha wakabapl
                                 otherfields ssachLengthLimit
            beforeSleep <- liftIO getPOSIXTime
            let canPost = beforeSleep - lposttime >= ssachPostTimeout board
            when (mode /= CreateNew && not canPost) $ do
                let slptime = (lposttime + ssachPostTimeout board) - beforeSleep
                blastLog $ "sleeping " ++ show slptime ++ " seconds before post. FIXME using threadDelay for sleeping, instead of a more precise timer"
                liftIO $ threadDelay $ round $ slptime * 1000000
            blastLog "posting"
            beforePost <- liftIO $ getPOSIXTime
            (out, _) <- blast $ post p
            -- FIXME afterPost <- liftIO $ getPOSIXTime
            blastOut (OutcomeMessage out)
            when (successOutcome out) $ blastLog "post succeded"
            let (nthreadtime, nposttime) =
                    if mode == CreateNew
                        then (beforePost, lposttime)
                        else (lthreadtime, beforePost)
                ret = return (nthreadtime, nposttime)
            case out of
                Success -> ret
                SuccessLongPost rest ->
                    if mode /= CreateNew
                        then blastPost cap nthreadtime nposttime w mode thread
                                (PostData "" rest Nothing (sageMode mode) False (escapeInv postdata) (escapeWrd postdata))
                        else ret
                TooFastPost -> do
                    blastLog "TooFastPost, retrying in 0.5 seconds"
                    return (lthreadtime, beforePost - (ssachPostTimeout board - 0.5))
                TooFastThread -> do
                    blastLog "TooFastThread, retrying in 15 minutes"
                    return (beforePost - (ssachThreadTimeout board / 2), lposttime)
                PostRejected -> do
                    blastLog "PostRejected, retrying later..."
                    return (lthreadtime, lposttime)
                o | o==NeedCaptcha || o==WrongCaptcha -> do
                    blastLog $ show o ++ ", requerying"
                    liftIO . reportbad =<< genOriginStamp
                    blastPost True lthreadtime lposttime w mode thread postdata
                  | otherwise -> do
                    blastLog "post failed"
                    ret

blastLoop :: (String, [Field]) -> POSIXTime -> POSIXTime -> BlastLog ()
blastLoop w lthreadtime lposttime = do
    (board, ShSettings{..}, MuSettings{..}) <- askBSM
    let getPage p = do
            let url = ssachPage board p
            let chkStatus _ _ = Nothing
            blastLog $ "getPage: going to page " ++ show p
            blastCloudflare (return . parsePage board . responseBody)
                (blast $ httpReqStrTags $
                    (fromJust $ parseUrl url){checkStatus=chkStatus}) url
    let getThread i = do
            blastLog $ "Going into " ++ show i ++ " thread for pasta"
            blast $ headNote "PastaHead fail" . fst . parseThreads <$> httpGetStrTags (ssachThread board (Just i))
    now <- liftIO $ getPOSIXTime
    canmakethread <- ifM (liftIO $ readTVarIO tcreatethreads)
                        (return $ now - lthreadtime >= ssachThreadTimeout board)
                        (return False)
    mp0 <- flMaybeSTM mthread (const $ return Nothing) $ do
                blastLog "mp0: Getting first page."
                Just <$> getPage 0
    flip (maybe $ blastLog "Thread chosen, ommitting page parsing") mp0 $ \p0 ->
        blastLog $ "page params:\n" ++
                   "page id: " ++ show (pageId p0) ++ "\n" ++
                   "lastpage id: " ++ show (lastpage p0) ++ "\n" ++
                   "speed: " ++ show (speed p0) ++ "\n" ++
                   "threads: " ++ show (length $ threads p0) ++ "\n" ++
                   "max replies: " ++ maybe "COULDN'T PARSE THREADS, EXPECT CRASH IN 1,2,3..." show (maximumMay $ map postcount $ threads p0)
    mode <- flMaybeSTM mmode return $
        maybe (do blastLog "No page, choosing from SagePopular/BumpUnpopular"
                  chooseFromList [SagePopular, BumpUnpopular])
              (\p0 -> do blastLog "Choosing mode..."
                         chooseMode board canmakethread p0) mp0
    recMode mode
    blastLog $ "chose mode " ++ show mode
    (thread, mpastapage) <- flMaybeSTM mthread (\t -> return (Just t, Nothing)) $ do
        blastLog "Choosing thread..."
        second Just <$> chooseThread board mode getPage
            (fromMaybe (error "Page is Nothing while thread specified") mp0)
    recThread thread
    blastLog $ "chose thread " ++ show thread
    postdata <- blastPostData mode getThread mpastapage thread
    (nthreadtime, nposttime) <- blastPost False lthreadtime lposttime w mode thread postdata
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
entryPoint :: BlastProxy -> Board -> LogDetail -> ShSettings -> MuSettings -> ProxySettings -> (OutMessage -> IO ()) -> Blast ()
entryPoint proxy board lgDetail shS muS prS output = do
    runBlastLog (BlastLogData proxy board lgDetail shS muS prS output) $ do
        blastLog "Entry point"
        blast $ httpSetProxy proxy
        let hands =
              [Handler $ \(a::AsyncException) -> do
                blastLog $ "Got async " ++ show a
                throwIO a
              {-,Handler $ \(a::HttpException) -> do
                blastLog $ "Got http exception, restarting. Exception was: " ++ show a
                start -- Dunno what to do except restart.-}
              ,Handler $ \(_::AbortOutcome) -> start --HACK abortOutcome
              ,Handler $ \(a::SomeException) -> do
                blastLog $ "Terminated by exception " ++ show a
                blastOut $ OutcomeMessage $ InternalError $ ErrorException a
              ]
            start = flip catches hands $
                blastLoop (ssachLastRecordedWakabaplAndFields (ssachBoard board)) 0 0
        start{-
        let url = ssachBoard board
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
sortSsachBoardsByPopularity boards = runBlast $ do
    maybeb <- forM boards $ \b -> do
                liftIO $ putStr $ "Processing " ++ renderBoard b ++ ". Speed: "
                spd <- parseSpeed <$> httpGetStrTags (ssachBoard b)
                liftIO $ putStrLn $ show spd
                return (b, spd)
    let (got, failed) = partition (isJust . snd) maybeb
        sorted = reverse $ sortBy (\(_,a) (_,b) -> compare (fromJust a) (fromJust b)) got
    return (map (second fromJust) sorted, fst $ unzip $ failed)
