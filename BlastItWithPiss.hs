module BlastItWithPiss where
import Import
import BlastItWithPiss.Blast
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Post
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Text.HTML.TagSoup
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.Generic as UTF8

import Control.Concurrent (forkIO)
import GHC.Conc (threadStatus, ThreadStatus(..))
import System.Process
import System.Exit
import Network

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
entryPoint :: ShSettings -> TQueue OutMessage -> Board -> MuSettings -> IO ()
entryPoint ShSettings{..} output board MuSettings{..} = do
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
        blastPost lposttime cap w@(wakabapl, otherfields) mode thread postdata = do
            (chKey, captcha) <- if cap || mode==CreateNew
                                    then do auxlog thread $ "querying captcha"
                                            chKey <- getChallengeKey ssachRecaptchaKey
                                            (,) chKey <$> blastCaptcha wakabapl thread chKey
                                    else return ("", "")
            p <- prepare board thread postdata
                            chKey captcha wakabapl otherfields ssachLengthLimit
            nposttime <- liftIO getPOSIXTime
            when (mode/=CreateNew && not (nposttime - lposttime >= ssachPostTimeout board)) $ do
                let slptime = (lposttime + ssachPostTimeout board) - nposttime
                auxlog thread $ "sleeping " ++ show slptime ++ " seconds before post"
                liftIO $ threadDelay $ round $ slptime * 1000000
            auxlog thread $ "posting"
            beforePost <- liftIO $ getPOSIXTime
            out <- post p
            liftIO $ atomically $ writeTQueue output (OutcomeMessage thread out)
            let reportSuccess = auxlog thread "post succeded"
                ret = return (beforePost, out)
            case out of
                Success -> do reportSuccess
                              ret
                SuccessLongPost rest -> if mode /= CreateNew
                    then do reportSuccess
                            blastPost nposttime cap w mode thread
                                (PostData "" rest Nothing (sageMode mode) False)
                    else do reportSuccess; ret
                TooFastPost -> do auxlog thread "TooFastPost, retrying in 0.5 seconds"
                                  ret
                TooFastThread -> do auxlog thread "TooFastThread, retrying in 15 minutes"
                                    ret
                o | o==NeedCaptcha || o==WrongCaptcha -> do
                        auxlog thread $ show o ++ ", requerying"
                        blastPost lposttime True w mode thread postdata
                  | otherwise -> do
                        auxlog thread "post failed"
                        ret
        newLoop w lthreadtime lposttime = handle (\(_::HttpException) -> newLoop w lthreadtime lposttime) $ do
                                -- Dunno what to do except restart.
            now <- liftIO $ getPOSIXTime
            canmakethread <- ifM (liftIO $ readTVarIO tcreatethreads)
                                (return $ now - lthreadtime >= ssachThreadTimeout board)
                                (return False)
            p0 <- getPage 0
            mode <- whenRandomSTM mmode $ chooseMode board canmakethread p0
            thread <- whenRandomSTM mthread $ chooseThread mode getPage p0
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
            (beforePost, out) <- blastPost lposttime False w mode thread
                                (PostData "" pasta image (sageMode mode) watermark)
            if successOutcome out
                then newLoop w
                        (if mode==CreateNew
                            then beforePost
                            else lthreadtime)
                        (if mode /= CreateNew && not (sageMode mode)
                            then beforePost
                            else lposttime)
                else newLoop w
                        (if out==TooFastThread
                            then beforePost - (ssachThreadTimeout board / 2)
                            else lthreadtime)
                        (if out==TooFastPost
                            then beforePost - ((ssachPostTimeout board / 20) * 19)
                            else lposttime)

main :: IO ()
main = withSocketsDo $ do
    tpastas <- atomically $ newTVar $ [
                                    {-
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
    mthread <- atomically $ newTVar $ {-Random--} Always $ Just $ 20183
    mmode <- atomically $ newTVar $ Random
    to <- atomically $ newTQueue
    th <- forkIO (entryPoint ShSettings{..} to MDK MuSettings{..})
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
