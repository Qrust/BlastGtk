module BlastItWithPiss where
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
import qualified Text.Show as Show
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

{-
import Control.Concurrent (forkIO)
import GHC.Conc (threadStatus, ThreadStatus(..))
import System.Process
import System.Exit
import Network
import qualified Data.ByteString.Lazy as L
--}

data MaybeRandom a = Always a
                   | Random

data ShSettings = ShSettings {tpastas :: TVar [String]
                             ,timages :: TVar [FilePath]
                             ,tuseimages :: TVar Bool
                             ,tcreatethreads :: TVar Bool
                             ,tmakewatermark :: TVar Bool
                             }

data MuSettings = MuSettings {mthread :: TVar (MaybeRandom (Maybe Int))
                             ,mmode :: TVar (MaybeRandom Mode)
                             }

data CaptchaAnswer = Answer String
                   | ReloadCaptcha
                   | AbortCaptcha
    deriving (Eq, Show, Ord)

data OriginStamp = OriginStamp !POSIXTime !Board !Mode !(Maybe Int)

data Message = OutcomeMessage !Outcome
             | LogMessage !String
             | SupplyCaptcha {captchaBytes :: !LByteString
                             ,captchaSend :: !(CaptchaAnswer -> IO ())
                             }
             | NoPastas
             | NoImages

data OutMessage = OutMessage !OriginStamp !Message

-- memory hog (?)
-- TODO Either expose LogS to calling thread or replace it with stateT with mode and thread.
data LogSettings = LogS
                {gwakabapl :: TVar String
                ,gotherfields :: TVar [Field]
                ,gmode :: TVar Mode
                ,gthread :: TVar (Maybe Int)
                ,gimage :: TVar (Maybe Image)
                ,gpasta :: TVar String
                ,gchKey :: TVar String
                ,glthreadtime :: TVar POSIXTime
                ,glposttime :: TVar POSIXTime
                }

data LogDetail = Log
               | Don'tLog
    deriving (Eq, Show, Ord, Enum, Bounded)

data BlastLogData = BlastLogData
        {bldBoard :: Board
        ,bldLogD :: LogDetail
        ,bldLogS :: LogSettings
        ,bldShS :: ShSettings
        ,bldMuS :: MuSettings
        ,bldOut :: TQueue OutMessage -- ^ sendOut :: OutMessage -> IO () ?
        }

type BlastLog = ReaderT BlastLogData Blast

instance Show OriginStamp where
    show (OriginStamp time board mode thread) =
        show time ++ " " ++ renderBoard board ++ " " ++ show mode ++ " [| " ++
        maybe (ssachBoard board) (ssachThread board) thread ++ " |]"

instance Show Message where
    show (OutcomeMessage o) = show o
    show (LogMessage o) = o
    show (SupplyCaptcha _ _) = "SupplyCaptcha"
    show NoPastas = "NoPastas"
    show NoImages = "NoImages"

instance Show OutMessage where
    show (OutMessage s m) = show s ++ ": " ++ show m

whenRandomSTM :: MonadIO m => TVar (MaybeRandom a) -> m a -> m a
whenRandomSTM t m = do
    r <- liftIO $ readTVarIO t
    case r of
        Always a -> return a
        Random -> m

defLogS :: (String, [Field]) -> IO LogSettings
defLogS (wakabpl, otherfields) = atomically $ do
    gwakabapl <- newTVar wakabpl
    gotherfields <- newTVar otherfields
    gmode <- newTVar CreateNew
    gthread <- newTVar Nothing
    gimage <- newTVar Nothing
    gpasta <- newTVar ""
    gchKey <- newTVar ""
    glthreadtime <- newTVar 0
    glposttime <- newTVar 0
    return LogS{..}

askBoard :: BlastLog Board
askBoard = asks bldBoard

askLogD :: BlastLog LogDetail
askLogD = asks bldLogD

askLogS :: BlastLog LogSettings
askLogS = asks bldLogS

askShS :: BlastLog ShSettings
askShS = asks bldShS

askBLSM :: BlastLog (Board, LogSettings, ShSettings, MuSettings)
askBLSM = asks $ \b -> (bldBoard b, bldLogS b, bldShS b, bldMuS b)

askOut :: BlastLog (TQueue OutMessage)
askOut = asks bldOut

rec :: TVar a -> a -> BlastLog a
rec t a = do
    d <- askLogD
    when (d == Log) $
        liftIO $ atomically $ writeTVar t a
    return a 

recM :: TVar a -> BlastLog a -> BlastLog a
recM t m = rec t =<< m

blastOut :: Message -> BlastLog ()
blastOut msg = do
    to <- askOut
    board <- askBoard
    LogS{..} <- askLogS
    liftIO $ do
        (mode, thread) <- (,) <$> readTVarIO gmode <*> readTVarIO gthread
        now <- getPOSIXTime
        atomically $ writeTQueue to $
            OutMessage (OriginStamp now board mode thread) msg

blastLog :: String -> BlastLog ()
blastLog msg = do
    d <- askLogD
    when (d == Log) $ do
        blastOut (LogMessage msg)

blast :: Blast a -> BlastLog a
blast = lift

blastImage :: Mode -> BlastLog (Maybe Image)
blastImage mode = do
    ShSettings{..} <- askShS
    use <- liftIO $ readTVarIO tuseimages
    if not use && mode /= CreateNew || mode == SagePopular
        then
            return Nothing
        else do
            images <- liftIO $ readTVarIO timages
            if null images
                then do
                    blastOut NoImages
                    blastLog "threw NoImages"
                    -- use recaptcha as a fallback
                    blast $ Just . Image "haruhi.jpg" "image/jpeg" <$>
                            (getCaptchaImage =<<
                                    getChallengeKey ssachRecaptchaKey)
                else blast $
                        Just <$> (readImageWithoutJunk =<< chooseFromList images)

blastPasta :: Maybe Image -> BlastLog String
blastPasta image = do
    ShSettings{..} <- askShS
    pastas <- liftIO $ readTVarIO tpastas
    if null pastas
        then do when (isNothing image) $ do
                    blastOut NoPastas
                    blastLog "threw NoPastas"
                return ""
        else chooseFromList pastas

blastCaptcha :: String -> Maybe Int -> BlastLog (String, Maybe String)
blastCaptcha wakabapl thread = do
    chKey <- blast $ getChallengeKey ssachRecaptchaKey
    mbbytes <- blast $ getCaptcha wakabapl thread ssachRecaptchaKey chKey
    case mbbytes of
        Nothing -> return (chKey, Just "")
        Just bytes -> do
            m <- newEmptyMVar
            blastOut $ SupplyCaptcha bytes (putMVar m)
            blastLog "blocking on captcha mvar"
            a <- takeMVar m
            blastLog "got captcha mvar"
            case a of
                Answer s -> return (chKey, Just s)
                ReloadCaptcha -> blastCaptcha wakabapl thread
                AbortCaptcha -> return (chKey, Nothing)

blastPost :: Bool -> POSIXTime -> POSIXTime -> (String, [Field]) -> Mode -> Maybe Int -> PostData -> BlastLog (POSIXTime, POSIXTime)
blastPost cap lthreadtime lposttime w@(wakabapl, otherfields) mode thread postdata = do
    (board, LogS{..}, ShSettings{..}, _) <- askBLSM
    (chKey, mcap) <- if cap || mode==CreateNew || not ssachAdaptivity
                        then do blastLog "querying captcha"
                                blastCaptcha wakabapl thread
                        else return ("", Just "")
    case mcap of
        Nothing -> return (lthreadtime, lposttime)
        Just captcha -> do
            void $ rec gchKey chKey
            p <- blast $ prepare board thread postdata chKey captcha wakabapl
                                 otherfields ssachLengthLimit
            beforeSleep <- liftIO getPOSIXTime
            let canPost = beforeSleep - lposttime >= ssachPostTimeout board
            when (mode /= CreateNew && not canPost) $ do
                let slptime = (lposttime + ssachPostTimeout board) - beforeSleep
                blastLog $ "sleeping " ++ show slptime ++ " seconds before post. FIXME using threadDelay for sleeping, instead of a more precise timer"
                -- FIXME precise timing
                liftIO $ threadDelay $ round $ slptime * 1000000
            blastLog "posting"
            -- FIXME beforePost <- liftIO $ getPOSIXTime
            out <- blast $ post p
            afterPost <- liftIO $ getPOSIXTime
            blastOut (OutcomeMessage out)
            when (successOutcome out) $ blastLog "post succeded"
            let (nthreadtime, nposttime) =
                    if mode == CreateNew
                        then (afterPost, lposttime)
                        else (lthreadtime, afterPost)
                ret = return (nthreadtime, nposttime)
            case out of
                Success -> ret
                SuccessLongPost rest ->
                    if mode /= CreateNew
                        then blastPost cap nthreadtime nposttime w mode thread
                                (PostData "" rest Nothing (sageMode mode) False)
                        else ret
                TooFastPost -> do
                        blastLog "TooFastPost, retrying in 0.5 seconds"
                        return (lthreadtime, afterPost - (ssachPostTimeout board - 0.5))
                TooFastThread -> do
                        blastLog "TooFastThread, retrying in 15 minutes"
                        return (afterPost - (ssachThreadTimeout board / 2), lposttime)
                o | o==NeedCaptcha || o==WrongCaptcha -> do
                        blastLog $ show o ++ ", requerying"
                        blastPost True lthreadtime lposttime w mode thread postdata
                  | otherwise -> do
                        blastLog "post failed"
                        ret

blastLoop :: (String, [Field]) -> POSIXTime -> POSIXTime -> BlastLog ()
blastLoop w lthreadtime lposttime = do
    let hands =
          [Handler $ \(_::HttpException) -> blastLoop w lthreadtime lposttime
                          -- Dunno what to do except restart.
          ,Handler $ \(a::AsyncException) -> throwIO a
          ,Handler $ \(a::SomeException) -> do
                blastLog $ "Terminated by exception " ++ show a
                throwIO a
          ]
    flip catches hands $ do
        (board, LogS{..}, ShSettings{..}, MuSettings{..}) <- askBLSM
        now <- liftIO $ getPOSIXTime
        canmakethread <- ifM (liftIO $ readTVarIO tcreatethreads)
                            (return $ now - lthreadtime >= ssachThreadTimeout board)
                            (return False)
        let getPage p = blast $ (parsePage board $!!) <$> httpGetStrTags (ssachPage board p)
        p0 <- getPage 0
        --p0 <- return $ Page 0 0 90000 [Thread 19947 True False 9000 []]
        blastLog $ "page params" ++ show (pageId p0, lastpage p0, speed p0, length $ threads p0)
        mode <- recM gmode $
                    whenRandomSTM mmode $ chooseMode board canmakethread p0
        thread <- recM gthread $
                    whenRandomSTM mthread $ chooseThread mode getPage p0
        blastLog $ "chose mode " ++ show mode
        rimage <- blastImage mode
        image <- recM gimage $
                    maybe (return Nothing) (\i -> Just <$> appendJunk i) rimage
        pasta <- recM gpasta $
                    blastPasta image
        watermark <- liftIO $ readTVarIO tmakewatermark
        (nthreadtime, nposttime) <-
            blastPost False lthreadtime lposttime w mode thread
                            (PostData "" pasta image (sageMode mode) watermark)
        void $ rec glthreadtime nthreadtime
        void $ rec glposttime nposttime
        blastLoop w nthreadtime nposttime

-- | Entry point should always be forked.
--
-- > thread <- forkIO (entryPoint print sh to Board ms)
--
-- You might want to resurrect thread if it gets killed.
--
-- > st <- threadStatus thread
-- > if st==ThreadDied || st==ThreadFinished
-- >    then resurrect
-- >    else continue
entryPoint :: LogDetail -> ShSettings -> TQueue OutMessage -> Board -> MuSettings -> Blast ()
entryPoint lgDetail shS output board muS = do
    x <- try $ parseForm ssach <$> httpGetStrTags (ssachPage board 0)
    case x of
        Left (a::SomeException) -> do
            now <- liftIO $ getPOSIXTime
            liftIO $ atomically $ writeTQueue output $
                OutMessage (OriginStamp now board CreateNew Nothing)
                           (LogMessage $ "Couldn't parse page form, got exception " ++ show a)
            throwIO a
        Right w -> do
            l <- liftIO $ defLogS w
            runReaderT (blastLoop w 0 0) $
                BlastLogData board
                             lgDetail
                             l
                             shS
                             muS
                             output

sortSsachBoardsByPopularity :: [Board] -> IO ([(Board, Int)], [Board])
sortSsachBoardsByPopularity boards = runBlast $ do
    maybeb <- forM boards $ \b -> do
                liftIO $ putStr $ "Processing " ++ renderBoard b ++ ". Speed: "
                spd <- parseSpeed <$> httpGetStrTags (ssachBoard b)
                liftIO $ putStrLn $ show spd
                return (b, spd)
    let (got, failed) = partition (isJust . snd) maybeb
        sorted = reverse $ sortBy (\(_,a) (_,b) -> compare (fromJust a) (fromJust b)) got
    return (map (appsnd fromJust) sorted, fst $ unzip $ failed)
{-
main :: IO ()
main = withSocketsDo $ do
    tpastas <- atomically $ newTVar $ [
                                    --{-
                                       "Я уже завтракал, мам"
                                      ,"Я у мамы молодец"
                                      ,"Долблюсь в попотан"
                                      ,"Ебать, спортсмен не выпал ни одного раза"
                                      ,concat $ replicate 600 "Длинный пост "
                                      ,"Слава Україні, Героям Слава!"-- -}
                                      ]
    timages <- atomically $ newTVar ["images/jew-swede.jpg"]
    tuseimages <- atomically $ newTVar False
    tcreatethreads <- atomically $ newTVar False
    tmakewatermark <- atomically $ newTVar False
    mthread <- atomically $ newTVar $ Random
    mmode <- atomically $ newTVar $ Random
    to <- atomically $ newTQueue
    th <- forkIO (entryPoint Log ShSettings{..} to MDK MuSettings{..})
    forever $ do
        t <- atomically $ tryReadTQueue to
        case t of{-
            Just x -> case x of
                OutcomeMessage thread (SuccessLongPost _) -> putStrLn $ show thread ++ " " ++ "SuccessLongPost"
                OutcomeMessage thread out -> putStrLn $ show thread ++ " " ++ show out
                SupplyCaptcha bytes send -> do
                    L.writeFile "captcha.jpeg" bytes
                    let captchabin =
#ifdef mingw32_HOST_OS
                            ".\\captcha.exe"
#else
                            "./captcha"
#endif
                    c <- readProcess captchabin ["captcha.jpeg"] []
                    send (Answer c)
                NoPastas -> putStrLn "NoPastas"
                NoImages -> putStrLn "NoImages"-}
            Nothing -> do st <- threadStatus th
                          if st==ThreadDied || st==ThreadFinished
                            then exitFailure
                            else threadDelay (2 * 1000000)
--}
