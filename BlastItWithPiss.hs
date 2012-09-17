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
import Text.HTML.TagSoup
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Text.Show as Show
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

{-import Control.Concurrent (forkIO)
import GHC.Conc (threadStatus, ThreadStatus(..))
import System.Process
import System.Exit
import Network-}

-- TODO support ANTIGATE, CAPTCHABOT, etc. add multipart/form-data to http-conduit
-- TODO support PROXYs. (It's more about frontend than library,
--                       library only provides API for one agent(proxy) anyway.)
-- TODO Switch to normal logging.
-- TODO entry point for proxy checker

-- FIXME Oh dog, what a mess.
-- TODO More type safety.
-- TODO Less boilerplate, less explicit parameter passing. (Enhance Blast monad)
-- TODO Increase modularity.
-- TODO Support 2chnu, alterchan.

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
                            -- TODO mcurrentpost with current post settings, e.g mode
                            --      thread, lposttime and the like. for debugging.
                             }

data CaptchaAnswer = Answer String
                   | ReloadCaptcha
                   | AbortCaptcha

data OutMessage = OutcomeMessage !(Maybe Int) !Outcome
                | SupplyCaptcha {captchaBytes :: !LByteString
                                ,captchaSend :: !(CaptchaAnswer -> IO ())
                                }
                | NoPastas
                | NoImages

-- memory hog (?)
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

data LogMessage = Lg !POSIXTime !Board !Mode !(Maybe Int) String

data LogDetail = Log
               | Don'tLog
    deriving (Eq, Show, Ord, Enum, Bounded)

data BlastLogData = BlastLogData
        {bldBoard :: Board
        ,bldLogD :: LogDetail
        ,bldLogS :: LogSettings
        ,bldSendLog :: LogMessage -> IO ()
        ,bldShS :: ShSettings
        ,bldMuS :: MuSettings
        ,bldOut :: TQueue OutMessage -- ^ sendOut :: OutMessage -> IO () ?
        }

type BlastLog = ReaderT BlastLogData Blast

instance Show LogMessage where
    show (Lg time board mode thread msg) =
        show time ++ " " ++ renderBoard board ++ " " ++ show mode ++ " [| " ++
        maybe (ssachBoard board) (ssachThread board) thread ++ " |]: " ++ msg

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

askSendLog :: BlastLog (LogMessage -> IO ())
askSendLog = asks bldSendLog

rec :: TVar a -> a -> BlastLog a
rec t a = do
    d <- askLogD
    when (d == Log) $
        liftIO $ atomically $ writeTVar t a
    return a 

recM :: TVar a -> BlastLog a -> BlastLog a
recM t m = do
    a <- m
    d <- askLogD
    when (d == Log) $
        liftIO $ atomically $ writeTVar t a
    return a 

blastLog :: String -> BlastLog ()
blastLog msg = do
    d <- askLogD
    when (d == Log) $ do
        board <- askBoard
        sendLog <- askSendLog
        LogS{..} <- askLogS
        liftIO $ do
            (mode, thread) <- (,) <$> readTVarIO gmode <*> readTVarIO gthread
            now <- getPOSIXTime
            sendLog $ Lg now board mode thread msg

blastOut :: OutMessage -> BlastLog ()
blastOut o = do
    to <- askOut
    liftIO $ atomically $ writeTQueue to o

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
                    blast $ Just . Image "haruhi.jpg" "image/jpeg" .
                        S.concat . L.toChunks
                            <$> (getCaptchaImage =<<
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

blastCaptcha :: String -> Maybe Int -> String -> BlastLog (Maybe String)
blastCaptcha wakabapl thread chKey = do
    mbbytes <- blast $ getCaptcha wakabapl thread ssachRecaptchaKey chKey
    case mbbytes of
        Nothing -> return $ Just ""
        Just bytes -> do
            m <- newEmptyMVar
            blastOut $ SupplyCaptcha bytes (putMVar m)
            blastLog "blocking on captcha mvar"
            a <- takeMVar m
            blastLog "got captcha mvar"
            case a of
                Answer s -> return $ Just s
                ReloadCaptcha -> blastCaptcha wakabapl thread chKey
                AbortCaptcha -> return Nothing

blastPost :: Bool -> POSIXTime -> POSIXTime -> (String, [Field]) -> Mode -> Maybe Int -> PostData -> BlastLog (POSIXTime, POSIXTime)
blastPost cap lthreadtime lposttime w@(wakabapl, otherfields) mode thread postdata = do
    (board, LogS{..}, ShSettings{..}, _) <- askBLSM
    (chKey, mcap) <- if cap || mode==CreateNew
                        then do blastLog "querying captcha"
                                chKey <- blast $ getChallengeKey ssachRecaptchaKey
                                (,) chKey <$> blastCaptcha wakabapl thread chKey
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
                blastLog $ "sleeping " ++ show slptime ++ " seconds before post"
                liftIO $ threadDelay $ round $ slptime * 1000000
            blastLog "posting"
            beforePost <- liftIO $ getPOSIXTime
            out <- blast $ post p
            blastOut $ OutcomeMessage thread out
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
                                (PostData "" rest Nothing (sageMode mode) False)
                        else ret
                TooFastPost -> do
                        blastLog "TooFastPost, retrying in 0.5 seconds"
                        ret
                TooFastThread -> do
                        blastLog "TooFastThread, retrying in 15 minutes"
                        ret
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
        let getPage p = blast $ parsePage board <$> httpGetStrTags (ssachPage board p)
        p0 <- getPage 0
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
entryPoint :: LogDetail -> (LogMessage -> IO ()) -> ShSettings -> TQueue OutMessage -> Board -> MuSettings -> IO ()
entryPoint lgDetail sendLog shS output board muS = do
    x <- try $ parseForm ssach . parseTags . UTF8.toString <$> simpleHttp (ssachPage board 0)
    case x of
        Left (a::SomeException) -> do now <- getPOSIXTime
                                      sendLog $ Lg now board CreateNew Nothing $
                                                "Couldn't parse page form, got exception " ++
                                                show a
                                      --throwIO a
        Right w -> do
            l <- defLogS w
            runBlast $ runReaderT (blastLoop w 0 0) $
                BlastLogData board
                             lgDetail
                             l
                             sendLog
                             shS
                             muS
                             output

sortSsachBoardsByPopularity :: [Board] -> IO ([(Board, Int)], [Board])
sortSsachBoardsByPopularity boards = runBlast $ do
    let boards = [minBound..maxBound]
    maybeb <- forM boards $ \b -> do
                liftIO $ putStr $ "Processing " ++ renderBoard b ++ ". Speed: "
                spd <- parseSpeed <$> httpGetStrTags (ssachBoard b)
                liftIO $ putStrLn $ show spd
                return (b, spd)
    let (got, fail) = partition (isJust . snd) maybeb
        sorted = reverse $ sortBy (\(_,a) (_,b) -> compare (fromJust a) (fromJust b)) got
    return (map (appsnd fromJust) sorted, fst $ unzip $ fail)
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
    tuseimages <- atomically $ newTVar True
    tcreatethreads <- atomically $ newTVar False
    tmakewatermark <- atomically $ newTVar False
    mthread <- atomically $ newTVar $ Random
    mmode <- atomically $ newTVar $ Random
    to <- atomically $ newTQueue
    th <- forkIO (entryPoint Log print ShSettings{..} to MDK MuSettings{..})
    forever $ do
        t <- atomically $ tryReadTQueue to
        case t of
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
                NoImages -> putStrLn "NoImages"
            Nothing -> do st <- threadStatus th
                          if st==ThreadDied || st==ThreadFinished
                            then exitFailure
                            else threadDelay (2 * 1000000)-}
