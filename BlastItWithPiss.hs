module BlastItWithPiss where
import Import
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.Post
import Control.Concurrent
import Control.Concurrent.STM
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.Generic as UTF8
import Data.Time.Clock.POSIX

import System.Process

-- FIXME FIXME FIXME simpleHTTP throws exceptions (like ResponseTimeout) and kills program
-- FIXME Oh dog, what a mess.
-- TODO switch to Browser. Add more generic header handling to browser.
-- TODO More type safety.
-- TODO Less boilerplate, less explicit parameter passing. (Rollout some monad)
-- TODO Increase modularity.
-- TODO Support non-makaba-boards. Add 2chnu, alterchan.

data MaybeRandom a = Always a
                   | Random

data MuSettings = MuSettings {tpastas :: TVar [String]
                             ,timages :: TVar [Image]
                             ,tuseimages :: TVar Bool
                             ,tcreatethreads :: TVar Bool
                             ,tthread :: TVar (MaybeRandom (Maybe Int))
                             ,tmode :: TVar (MaybeRandom Mode)}

data CaptchaAnswer = Answer String
                   | ReloadCaptcha

data OutMessage = OutcomeMessage (Maybe Int) Outcome
                | SupplyCaptcha {captchaBytes :: LByteString
                                ,captchaSend :: (CaptchaAnswer -> IO ())
                                } -- we send outcome anyway
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

downloadPage :: Board -> Int -> IO Page
downloadPage b i = parsePage <$> downloadTags (ssachPage b i)

-- | Entry point should always be forked.
-- > forkIO (entryPoint MDK ts to)
entryPoint :: Board -> MuSettings -> TQueue OutMessage -> IO ()
entryPoint board MuSettings{..} output = do
    w <- parseForm ssach <$> downloadTags (ssachPage board 0)
    newLoop w 0
  where --kokoko
        auxlog thread m = do
            now <- getPOSIXTime
            putStrLn $ show now ++ " " ++ show board ++ " | " ++ show thread ++ ": " ++ m
        getPage = downloadPage board
        blastCaptcha wakabapl thread chKey = do
            mbbytes <- getCaptcha wakabapl thread ssachRecaptchaKey chKey
            case mbbytes of
                Nothing -> return ""
                Just bytes -> do
                    m <- newEmptyMVar
                    atomically $
                        writeTQueue output $ SupplyCaptcha bytes (putMVar m)
                    auxlog thread "blocking on mvar"
                    a <- takeMVar m
                    auxlog thread "got mvar"
                    case a of
                        Answer s -> return s
                        ReloadCaptcha -> blastCaptcha wakabapl thread chKey
        blastPost w@(wakabapl, otherfields) mode thread postdata = do
            chKey <- getChallengeKey ssachRecaptchaKey
            captcha <- blastCaptcha wakabapl thread chKey
            p <- prepare board thread postdata
                            chKey captcha wakabapl otherfields ssachLengthLimit
            beforePost <- getPOSIXTime
            out <- post p -- FIXME for some reason post eats up 2 seconds for nothing.
            atomically $ writeTQueue output (OutcomeMessage thread out)
            let ret = return (beforePost, out)
                success = do afterPost <- getPOSIXTime
                             auxlog thread "sleeping 10 seconds after success"
                             threadDelay ((10 - round (afterPost - beforePost)) * 1000000)
                             ret
                failure = do auxlog thread "sleeping 2 seconds after fail"
                             threadDelay (2 * 1000000)
                             ret
            case out of
                Success -> success
                SuccessLongPost rest -> if mode /= CreateNew
                    then do _ <- success
                            blastPost w mode thread
                                    (PostData "" rest Nothing (sageMode mode) False)
                    else success
                NeedCaptcha -> do auxlog thread $
                                    "don't know how to handle NeedCaptcha, we "
                                    ++ "always query for captcha anyway"
                                  ret
                WrongCaptcha -> ret
                TooFastThread -> do auxlog thread $ "too fast thread, retrying in 15 minutes"
                                    ret
                _ -> failure
        newLoop w lthreadtime = do
            now <- getPOSIXTime
            canmakethread <- ifM (readTVarIO tcreatethreads)
                                (return $ now - lthreadtime >= realToFrac (ssachThreadTimeout board))
                                (return False)
            p0 <- getPage 0
            mode <- whenRandomSTM tmode $ chooseMode board canmakethread p0
            thread <- whenRandomSTM tthread $ chooseThread mode getPage p0
            auxlog thread $ "chose mode " ++ show mode
            rimage <- do use <- readTVarIO tuseimages
                         if not use && mode /= CreateNew
                            then return Nothing
                            else do images <- readTVarIO timages
                                    if null images
                                        then do
                                            atomically $ writeTQueue output $ NoImages
                                            auxlog thread "threw NoImages"
                                            -- as a fallback use recaptcha image
                                            chKey <- getChallengeKey ssachRecaptchaKey
                                            Just . Image "haruhi.jpg" "image/jpeg" .
                                                S.concat . L.toChunks
                                                    <$> getCaptchaImage chKey
                                        else Just <$> chooseFromList images
            image <- maybe (return Nothing) (\i -> Just <$> appendJunk i) rimage
            pasta <- do pastas <- readTVarIO tpastas
                        if null pastas
                            then do when (isNothing image) $ do
                                        atomically $ writeTQueue output $ NoPastas
                                        auxlog thread "threw NoPastas"
                                    return ""
                            else chooseFromList pastas
            (beforePost, out) <- blastPost w mode thread (PostData "" pasta image (sageMode mode) False)
            if successOutcome out
                then newLoop w (if mode==CreateNew then beforePost else lthreadtime)
                else newLoop w (if out==TooFastThread
                                    then beforePost - (realToFrac (ssachThreadTimeout board) / 2)
                                    else lthreadtime)

main :: IO ()
main = do
    tpastas <- atomically $ newTVar []
    timages <- atomically . newTVar =<< mapM readImageWithoutJunk ["images/jew-swede.jpg"]
    tuseimages <- atomically $ newTVar True
    tcreatethreads <- atomically $ newTVar False
    tthread <- atomically $ newTVar (Always (Just 18189))
    tmode <- atomically $ newTVar Random
    to <- atomically $ newTQueue
    void $ forkIO (entryPoint MDK MuSettings{..} to)
    forever $ do
        x <- atomically $ readTQueue to
        case x of
            OutcomeMessage thread (SuccessLongPost _) -> putStrLn $ show thread ++ " " ++ "SuccessLongPost"
            OutcomeMessage thread out -> putStrLn $ show thread ++ " " ++ show out
            SupplyCaptcha bytes send -> do
                L.writeFile "captcha.jpeg" bytes
                c <- readProcess "./captcha" ["captcha.jpeg"] []
                send (Answer c)
            NoPastas -> putStrLn "NoPastas"
            NoImages -> putStrLn "NoImages"
