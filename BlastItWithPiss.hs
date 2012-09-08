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
--import Network.HTTP.Types
--import Network.Mime
import Network.HTTP.Conduit
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.Generic as UTF8
import Data.Time.Clock.POSIX

data MuSettings = MuSettings {tpastas :: TVar [String]
                             ,timages :: TVar [Image]
                             ,tuseimages :: TVar Bool}

data OutMessage = OutcomeMessage (Maybe Int) Outcome
                | SupplyCaptcha {captchaBytes :: LByteString
                                ,sendCaptcha :: String -> IO ()
                                ,reloadCaptcha :: IO (Maybe LByteString) -- ^ If Nothing send empty string
                                } -- we send outcome anyway
                | NoPastas
                | NoImages

downloadTags :: String -> IO [Tag String]
downloadTags u = parseTags . UTF8.toString <$> simpleHttp u

downloadPage :: Board -> Int -> IO Page
downloadPage b i = parsePage <$> downloadTags (ssachPage b i)

-- TODO switch to Browser. Add more generic header handling to browser.
-- TODO More type safety.
-- TODO tests — hspec, hunit.
-- | Entry point should always be forked.
-- > forkIO (entryPoint MDK ts to)
entryPoint :: Board -> MuSettings -> TQueue OutMessage -> IO ()
entryPoint board MuSettings{..} output = do
    w <- parseForm ssach <$> downloadTags (ssachPage board 0)
    newLoop w 0
  where getPage = downloadPage board
        newLoop w@(wakabapl, otherfields) lthreadtime = do
            now <- getPOSIXTime
            let canmakethread = now - lthreadtime >= realToFrac (ssachThreadTimeout board)
            p0 <- getPage 0
            mode <- chooseMode board canmakethread p0
            thread <- chooseThread mode getPage p0
            chKey <- getChallengeKey ssachRecaptchaKey
            mbbytes <- getCaptcha wakabapl thread ssachRecaptchaKey chKey
            captcha <- case mbbytes of
                        Nothing -> return ""
                        Just bytes -> do
                            m <- newEmptyMVar
                            atomically $ writeTQueue output $ SupplyCaptcha bytes
                                (putMVar m)
                                (getCaptcha wakabapl thread ssachRecaptchaKey chKey)
                            putStrLn $ show (board, thread) ++ ": blocking on mvar"
                            c <- takeMVar m
                            putStrLn $ show (board, thread) ++ ": got mvar"
                            return c
            pasta <- do pastas <- readTVarIO tpastas
                        if null pastas
                            then do atomically $ writeTQueue output $ NoPastas
                                    putStrLn $ show (board, thread) ++ ": threw NoPastas"
                                    return $ concat $ replicate 500 "NOPASTA"
                            else chooseFromList pastas
            rimage <- do use <- readTVarIO tuseimages
                         if not use && mode /= CreateNew
                            then return Nothing
                            else do images <- readTVarIO timages
                                    if null images
                                        then do
                                            atomically $ writeTQueue output $ NoImages
                                            putStrLn $ show (board, thread) ++ ": threw NoImages"
                                            -- as a fallback use recaptcha image
                                            Just . Image "haruhi.jpg" "image/jpeg" .
                                                S.concat . L.toChunks
                                                    <$> getCaptchaImage chKey
                                        else Just <$> chooseFromList images
            image <- maybe (return Nothing) (\i -> Just <$> appendJunk i) rimage
            p <- prepare board thread (PostData "" pasta image (sageMode mode) False)
                             chKey captcha wakabapl otherfields ssachLengthLimit
            beforePost <- getPOSIXTime
            out <- post p -- FIXME for some reason post eats up 2 seconds for nothing.
            atomically $ writeTQueue output (OutcomeMessage thread out)
            putStrLn $ show (board, thread) ++ ": sent outcome " ++ show out
            case out of
                Success -> newLoop w (if mode==CreateNew then beforePost
                                                         else lthreadtime)
                SuccessLongPost _ -> undefined
                _ -> undefined
