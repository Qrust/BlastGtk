module GtkBlast.Mainloop
    (wipebuttonEnvPart
    ,mainloop
    ,setMainLoop
    ) where
import Import hiding (on)
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.Log
import GtkBlast.EnvPart
import GtkBlast.Captcha
import GtkBlast.Pasta
import GtkBlast.Image
import GtkBlast.Proxy
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Blast
import "blast-it-with-piss" BlastItWithPiss.Parsing
import "blast-it-with-piss" BlastItWithPiss.Board
import Graphics.UI.Gtk hiding (get,set)
import Control.Concurrent
import GHC.Conc
import Control.Concurrent.STM
import qualified Data.Map as M
import Control.Monad.Trans.Maybe
import Paths_blast_it_with_piss

maintainWipeUnit :: BoardUnit -> Bool -> Bool -> WipeUnit -> E (Maybe WipeUnit)
maintainWipeUnit BoardUnit{..} isActive isWiping w@WipeUnit{..} = do
        E{..} <- ask
        st <- io $ threadStatus wuThreadId
        isBanned <- get wuBanned
        pxs <- M.keys <$> get proxies
        if st == ThreadDied || st == ThreadFinished
            then do
                writeLog $ "blasgtk: Thread for {" ++ show wuProxy ++ "} " ++ renderBoard buBoard ++ " died. Removing"
                return Nothing
            else if not isActive || not isWiping || isBanned || notElem wuProxy pxs
                    then do
                        writeLog $ "blasgtk: Killing thread for " ++ renderBoard buBoard
                        io $ killThread wuThreadId
                        return Nothing -- TODO don't regenerate banned threads
                    else return $ Just w

regenerateExcluding :: Board -> [WipeUnit] -> E [WipeUnit]
regenerateExcluding board exc = do
    E{..} <- ask
    prx <- M.assocs <$> get proxies
    when (null prx) $ tempError 2 "Нет проксей"
    catMaybes <$> forM prx (\(p, s) ->
        if any ((==p) . wuProxy) exc
            then return Nothing
            else do writeLog $ "Spawning new thread for " ++ renderBoard board ++ "{" ++ show p ++ "}"
                    mthread <- io $ atomically $ newTVar Nothing
                    mmode <- io $ atomically $ newTVar Nothing
                    threadid <- io $ forkIO $ runBlast $ do
                        --entryPoint p board Log shS MuSettings{..} s (putStrLn . show)
                        entryPoint p board Log shS MuSettings{..} s $ atomically . writeTQueue tqOut
                    writeLog $ "Spawned " ++ renderBoard board ++ "{" ++ show p ++ "}"
                    Just . WipeUnit p threadid <$> io (newIORef False)
        )
    
maintainBoardUnit :: (Int, Int) -> BoardUnit -> E (Int, Int)
maintainBoardUnit (active, banned) bu@BoardUnit{..} = do
    E{..} <- ask
    isActive <- get buWidget
    isWiping <- get wipeStarted
    new <- catMaybes <$> (mapM (maintainWipeUnit bu isActive isWiping) =<< (get buWipeUnits))
    regend <- if isActive && isWiping
                then regenerateExcluding buBoard new
                else return []
    set buWipeUnits $ new ++ regend
    isBanned <- --FIXME FIXME FIXME readIORef buBanned
                return False
    return (active + (if isActive then 1 else 0)
           ,banned + (if isBanned then 1 else 0))

maintainBoardUnits :: E ()
maintainBoardUnits = do
    E{..} <- ask
    (active, banned) <- foldM maintainBoardUnit (0,0) boardUnits
    set activeCount active
    set bannedCount banned

startWipe :: E ()    
startWipe = do
    E{..} <- ask
    writeLog "Starting wipe..."
    set wipeStarted True
    maintainBoardUnits

killWipe :: E ()
killWipe = do
    E{..} <- ask
    writeLog "Stopping wipe..."
    set wipeStarted False
    maintainBoardUnits
    killAllCaptcha

wipebuttonEnvPart :: Builder -> EnvPart
wipebuttonEnvPart b = EP
    (\env _ -> do
        wbuttonwipe <- builderGetObject b castToButton "wipebutton"

        void $ on wbuttonwipe buttonActivated $ do
            ifM (not <$> readIORef (wipeStarted env))
                (runE env $ do
                    E{..} <- ask
                    startWipe
                    io $ buttonSetLabel wbuttonwipe "Прекратить _Вайп"
                    io $ progressBarPulse wprogresswipe
                    updWipeMessage
                    )
                (runE env $ do
                    E{..} <- ask
                    killWipe
                    io $ buttonSetLabel wbuttonwipe "Начать _Вайп"
                    io $ progressBarSetFraction wprogresswipe 0
                    updMessage "Вайп ещё не начат"
                    )
        return wbuttonwipe
        )
    (const return)
    (const id)

setBanned :: [BoardUnit] -> Board -> BlastProxy -> Bool -> IO ()
setBanned boardUnits board proxy st = do
        maybe (return ()) ((`writeIORef` st) . wuBanned) =<< runMaybeT (do
            ws <- maybe mzero (liftIO . readIORef . buWipeUnits) $
                    find ((==board) . buBoard) boardUnits
            maybe mzero return $ find ((==proxy) . wuProxy) ws)

reactToMessage :: OutMessage -> E ()
reactToMessage s@(OutMessage st@(OriginStamp _ proxy board _ _) m) = do
    E{..} <- ask
    case m of
        OutcomeMessage o -> do
            case o of
                SuccessLongPost _ -> writeLog (show st ++ ": SuccessLongPost")
                _ -> writeLog (show s)
            case o of
                Success -> do
                    io $ modifyIORef postCount (+1)
                    io $ setBanned boardUnits board proxy False
                SuccessLongPost _ -> io $ modifyIORef postCount (+1)
                Wordfilter -> tempError 3 "Не удалось обойти вордфильтр"
                Banned x -> do
                    banMessage 5 $ "Забанен на доске " ++ renderCompactStamp st
                                ++ " Причина: " ++ show x
                                ++ "\nВозможно стоит переподключится или начать вайпать /d/"
                    io $ setBanned boardUnits board proxy True
                SameMessage -> tempError 2 $ renderCompactStamp st ++ ": Запостил одно и то же сообщение"
                SameImage -> tempError 2 $ renderCompactStamp st ++ ": Запостил одну и ту же пикчу"
                TooFastPost -> return () -- tempError 2 $ renderCompactStamp st ++ ": Вы постите слишком часто, умерьте пыл"
                TooFastThread -> tempError 3 $ renderCompactStamp st ++ ": Вы создаете треды слишком часто"
                NeedCaptcha -> return ()
                WrongCaptcha -> tempError 3 "Неправильно введена капча"
                RecaptchaBan -> do
                    banMessage 7 $ "Забанен рекапчой, охуеть. Переподключайся, мудило"
                    io $ setBanned boardUnits board proxy True
                LongPost -> tempError 1 $ renderCompactStamp st ++ ": Запостил слишком длинный пост"
                CorruptedImage -> tempError 2 $ renderCompactStamp st ++ ": Запостил поврежденное изображение"
                OtherError x -> tempError 7 $ renderCompactStamp st ++ ": " ++ show x
                InternalError x -> tempError 7 $ renderCompactStamp st ++ ": " ++ show x
                CloudflareCaptcha -> do
                    banMessage 7 $ "Если эта ошибка появляется то это баг, сообщите нам об этом"
                    io $ setBanned boardUnits board proxy True
                CloudflareBan -> do
                    banMessage 7 $ "Эту проксю пидорнули по клаудфлеру, она бесполезна"
                    io $ setBanned boardUnits board proxy True
                UnknownError -> tempError 4 $ renderCompactStamp st ++ ": Неизвестная ошибка, что-то пошло не так"
        c@SupplyCaptcha{} -> addCaptcha (st, c)
        LogMessage _ -> writeLog (show s)
        NoPastas -> do writeLog (show s)
                       tempError 3 "Невозможно прочитать пасты, постим повторяющуюся строку \"NOPASTA\""
        NoImages -> do writeLog (show s)
                       tempError 3 "Невозможно прочитать пикчи, постим капчу"

mainloop :: E ()
mainloop = do
    E{..} <- ask
    whenM (get wipeStarted) $ do
        maintainCaptcha
        regeneratePastaGen
        regenerateImages
        regenerateProxies
        maintainBoardUnits
        updWipeMessage
    mapM_ reactToMessage =<< (io $ atomically $ untilNothing $ tryReadTQueue tqOut)

setMainLoop :: Env -> FilePath -> (Conf -> IO Conf) -> IO ()
setMainLoop env configfile setConf = do
    void $ timeoutAddFull (do
        whenM (get $ wipeStarted env) $
            progressBarPulse $ wprogresswipe env
        True <$ yield) priorityDefaultIdle 10
    void $ timeoutAddFull (do
        runE env mainloop
        True <$ yield) priorityDefaultIdle 50 --kiloseconds, 20 fps.
    void $ onDestroy (window env) $ do
        runE env . writeConfig configfile =<< setConf def{coFirstLaunch=False, coLastVersion=version}
        mainQuit
