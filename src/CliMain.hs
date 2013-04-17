module Main (main) where
import Prelude
{-
import Control.Monad
import GHC.Conc
import Control.Concurrent.STM
import Network
import System.Exit
import System.Process
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Blast
import "blast-it-with-piss" BlastItWithPiss.Post
import "blast-it-with-piss" BlastItWithPiss.Board
import qualified Data.ByteString.Lazy as L
-}

main :: IO ()
main = return ()
{-
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
    mthread <- atomically $ newTVar $ Nothing
    mmode <- atomically $ newTVar $ Nothing
    to <- atomically $ newTQueue
    prs <- defPrS
    th <- forkIO (runBlast $ entryPoint MDK NoProxy Log ShSettings{..} MuSettings{..} prs to)
    forever $ do
        t <- atomically $ tryReadTQueue to
        case t of
            Just (OutMessage (OriginStamp _ _ _ _ thread) x) -> case x of
                OutcomeMessage (SuccessLongPost _) -> putStrLn $ show thread ++ " " ++ "SuccessLongPost"
                OutcomeMessage out -> putStrLn $ show thread ++ " " ++ show out
                SupplyCaptcha _ bytes send -> do
                    L.writeFile "captcha.jpeg" bytes
                    let captchabin =
#ifdef mingw32_HOST_OS
                            ".\\captcha.exe"
#else
                            "./captcha"
#endif
                    c <- readProcess captchabin ["captcha.jpeg"] []
                    send (Answer c)
                LogMessage s -> putStrLn s
                NoPastas -> putStrLn "NoPastas"
                NoImages -> putStrLn "NoImages"
            Nothing -> do st <- threadStatus th
                          if st==ThreadDied || st==ThreadFinished
                            then exitFailure
                            else threadDelay (2 * 1000000)
-}
