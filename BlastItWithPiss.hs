module BlastItWithPiss where
import Import
import BlastItWithPiss.Blast
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Post
import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Text.HTML.TagSoup
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.Generic as UTF8
import Data.Time.Clock.POSIX

import GHC.Conc (threadStatus, ThreadStatus(..))
import System.Process
import System.Exit
import Network

--TODO support ANTIGATE, CAPTCHABOT, etc.
--TODO add multipart/form-data to http-conduit

-- FIXME Oh dog, what a mess.
-- TODO More type safety.
-- TODO Less boilerplate, less explicit parameter passing. (Rollout some monad)
-- TODO Increase modularity.
-- TODO Support 2chnu, alterchan.
-- TODO Switch to normal logging.

data MaybeRandom a = Always a
                   | Random

data MuSettings = MuSettings {tpastas :: TVar [String]
                             ,timages :: TVar [FilePath]
                             ,tuseimages :: TVar Bool
                             ,tcreatethreads :: TVar Bool
                             ,tmakewatermark :: TVar Bool
                             ,tthread :: TVar (MaybeRandom (Maybe Int))
                             ,tmode :: TVar (MaybeRandom Mode)}

data CaptchaAnswer = Answer String
                   | ReloadCaptcha

data OutMessage = OutcomeMessage (Maybe Int) Outcome
                | SupplyCaptcha {captchaBytes :: LByteString
                                ,captchaSend :: (CaptchaAnswer -> IO ())
                                }
                | NoPastas
                | NoImages

whenRandomSTM :: MonadIO m => TVar (MaybeRandom a) -> m a -> m a
whenRandomSTM t m = do
    r <- liftIO $ readTVarIO t
    case r of
        Always a -> return a
        Random -> m

downloadTags :: String -> IO [Tag String]
downloadTags u = parseTags . UTF8.toString <$> simpleHttp u

-- | Entry point should always be forked.
--
-- > thread <- forkIO (entryPoint MDK ts to)
--
-- You might want to resurrect thread if it gets killed.
--
-- > st <- threadStatus thread
-- > if st==ThreadDied || st==ThreadFinished
-- >    then resurrect
-- >    else continue
entryPoint :: Board -> MuSettings -> TQueue OutMessage -> IO ()
entryPoint board MuSettings{..} output = do
    w <- parseForm ssach <$> downloadTags (ssachPage board 0)
    runBlast $ newLoop w 0 0
  where --kokoko ;_;
        auxlog thread m = liftIO $ do
            now <- getPOSIXTime
            putStrLn $ show now ++ " " ++ show board ++ " | " ++ show thread ++ ": " ++ m
        getPage p = parsePage <$> httpGetStrTags (ssachPage board p)
        blastCaptcha wakabapl thread chKey = do
            mbbytes <- getCaptcha wakabapl thread ssachRecaptchaKey chKey
            case mbbytes of
                Nothing -> return ""
                Just bytes -> do
                    m <- newEmptyMVar
                    liftIO $ atomically $
                        writeTQueue output $ SupplyCaptcha bytes (putMVar m)
                    auxlog thread "blocking on mvar"
                    a <- takeMVar m
                    auxlog thread "got mvar"
                    case a of
                        Answer s -> return s
                        ReloadCaptcha -> blastCaptcha wakabapl thread chKey
        blastPost cap w@(wakabapl, otherfields) mode thread postdata= do
            (chKey, captcha) <- if cap || mode==CreateNew
                                    then do auxlog thread $ "querying captcha"
                                            chKey <- getChallengeKey ssachRecaptchaKey
                                            (,) chKey <$> blastCaptcha wakabapl thread chKey
                                    else return ("", "")
            p <- prepare board thread postdata
                            chKey captcha wakabapl otherfields ssachLengthLimit
            auxlog thread $ "posting"
            beforePost <- liftIO $ getPOSIXTime
            out <- post p -- FIXME for some reason post eats up 3 seconds for nothing.
                          -- FIXME Not a lazyness issue(?)
            liftIO $ atomically $ writeTQueue output (OutcomeMessage thread out)
            let reportSuccess = do
                    afterPost <- liftIO $ getPOSIXTime
                    auxlog thread "sleeping 10 seconds after success"
                    liftIO $ threadDelay ((10 - round (afterPost - beforePost)) * 1000000)
                reportFail = do
                    auxlog thread "sleeping 1 second after fail"
                    liftIO $ threadDelay 1000000
            let ret = return (beforePost, out)
            case out of
                Success -> do reportSuccess
                              ret
                SuccessLongPost rest -> if mode /= CreateNew
                    then do reportSuccess
                            blastPost cap w mode thread
                                (PostData "" rest Nothing (sageMode mode) False)
                    else do reportSuccess; ret
                TooFastPost -> do auxlog thread "TooFastPost, retrying in 1 second"
                                  liftIO $ threadDelay 1000000
                                  blastPost cap w mode thread postdata
                TooFastThread -> do auxlog thread $ "TooFastThread, retrying in 15 minutes"
                                    ret
                o | o==NeedCaptcha || o==WrongCaptcha -> do
                        auxlog thread $ show o ++ ", requerying"
                        blastPost True w mode thread postdata
                  | otherwise -> do
                        reportFail
                        ret
        newLoop w lthreadtime lposttime = handle (\(_::HttpException) -> newLoop w lthreadtime lposttime) $ do
                                -- Dunno what to do except restart.
            now <- liftIO $ getPOSIXTime
            canmakethread <- ifM (liftIO $ readTVarIO tcreatethreads)
                                (return $ now - lthreadtime >= realToFrac (ssachThreadTimeout board))
                                (return False)
            p0 <- getPage 0
            mode <- whenRandomSTM tmode $ chooseMode board canmakethread p0
            thread <- whenRandomSTM tthread $ chooseThread mode getPage p0
            auxlog thread $ "chose mode " ++ show mode
            rimage <- do use <- liftIO $ readTVarIO tuseimages
                         if not use && mode /= CreateNew || mode == SagePopular
                            then return Nothing
                            else do images <- liftIO $ readTVarIO timages
                                    if null images
                                        then do
                                            liftIO $ atomically $
                                                writeTQueue output $ NoImages
                                            auxlog thread "threw NoImages"
                                            -- use recaptcha as a fallback
                                            chKey <- getChallengeKey ssachRecaptchaKey
                                            Just . Image "haruhi.jpg" "image/jpeg" .
                                                S.concat . L.toChunks
                                                    <$> getCaptchaImage chKey
                                        else do i <- chooseFromList images
                                                Just <$> readImageWithoutJunk i
            image <- maybe (return Nothing) (\i -> Just <$> appendJunk i) rimage
            pasta <- do pastas <- liftIO $ readTVarIO tpastas
                        if null pastas
                            then do when (isNothing image) $ do
                                        liftIO $ atomically $
                                            writeTQueue output $ NoPastas
                                        auxlog thread "threw NoPastas"
                                    return ""
                            else chooseFromList pastas
            watermark <- liftIO $ readTVarIO tmakewatermark
            (beforePost, out) <- blastPost False w mode thread
                                (PostData "" pasta image (sageMode mode) watermark)
            if successOutcome out
                then newLoop w
                        (if mode==CreateNew then beforePost else lthreadtime)
                        lposttime
                else newLoop w
                        (if out==TooFastThread
                            then beforePost - (realToFrac (ssachThreadTimeout board) / 2)
                            else lthreadtime)
                        lposttime

main :: IO ()
main = withSocketsDo $ do
    tpastas <- atomically $ newTVar $ [
                                    -- {-
                                       "Я уже завтракал, мам"
                                      ,"Я у мамы молодец"
                                      ,"Долблюсь в попотан"
                                      ,"Ебать, спортсмен не выпал ни одного раза"
                                      ,concat $ replicate 600 "Длинный пост "
                                      ,"Слава Україні, Героям Слава!"-- -}
                                      ]
    timages <- atomically $ newTVar ["images/jew-swede.jpg"]
    tuseimages <- atomically $ newTVar True
    tcreatethreads <- atomically $ newTVar True
    tmakewatermark <- atomically $ newTVar False
    tthread <- atomically $ newTVar Random
    tmode <- atomically $ newTVar Random
    to <- atomically $ newTQueue
    th <- forkIO (entryPoint MDK MuSettings{..} to)
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
                            else threadDelay (2 * 1000000)
        
