module GtkBlast.AntigateCaptcha
    (addAntigateCaptchas
    ,addAntigateCaptcha
    ,killAntigateCaptcha
    ,deactivateAntigateCaptcha
    ,antigateCaptchaEnvPart
    ,maintainAntigateCaptcha
    ) where
import Import hiding (on, mod)

import GtkBlast.Type_CaptchaMode
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Conf
import GtkBlast.EnvPart

import BlastItWithPiss.Board
import BlastItWithPiss.Blast
import BlastItWithPiss

import Text.Recognition.Antigate
import Graphics.UI.Gtk hiding (get, set)

import GHC.Conc
import Control.Concurrent.STM
import Data.Time.Clock.POSIX

import Control.Monad.Trans.Resource
import qualified Data.Map as M

antigateThread :: Manager -> (OriginStamp, SupplyCaptcha) -> TQueue (Either String String) -> ApiKey -> IO ()
antigateThread connection (st, SupplyCaptcha{..}) tq key =
    runResourceT $ flip catches hands $ do
        ut <- io $ newIORef 0
        now <- io getPOSIXTime
        let sconf =
              def {api_counter = counter ut now
                  ,api_upload_callback = upcallback ut now}

        (cid, str) <- solveCaptcha sconf key captchaConf captchaFilename
                        captchaBytes connection

        lg $ "Sending antigate answer \"" ++ str ++ "\" to " ++ renderCompactStamp st
        io $ captchaSend $ Answer str (handle errex . void . report cid)

        lg $ "Antigate thread finished for " ++ renderCompactStamp st
  where lg :: MonadIO m => String -> m ()
        lg = io . atomically . writeTQueue tq . Right
        upcallback uploadtime then' cid = do
            now <- getPOSIXTime
            set uploadtime now
            lg $ "Got captcha id for " ++ renderCompactStamp st ++ ": " ++
                show cid ++ ". Spent " ++ show (now-then') ++ " on uploading"
        counter _ _ _ 0 = return ()
        counter uploadtime then' phase c = do
            ut <- get uploadtime
            now <- getPOSIXTime
            lg $ "Retry #" ++ show c ++ " of " ++ show phase ++ " for " ++
                renderCompactStamp st ++ ": spent " ++
                    (case phase of
                        CheckPhase ->
                          show (now - then') ++ " solving captcha, " ++
                            "with " ++ show (now - ut) ++ " spent on checking"
                        UploadPhase -> show (now - then') ++ " on uploading"
                    )
        err :: MonadIO m => String -> m ()
        err = io . atomically . writeTQueue tq . Left
        report cid nst = do
            lg $ "Reporting bad captcha id " ++ show cid ++ " for " ++ renderCompactStamp nst
            runResourceT $ reportBad key cid connection
        errex (e::SomeException) = err $ "errex " ++ renderCompactStamp st ++ ": " ++ (show e)
        hands :: [Handler (ResourceT IO) ()]
        hands =
            [Handler $ \(e::SolveException) -> do
                case e of
                    SolveExceptionUpload a ->
                        err $ "Не удалось загрузить капчу на антигейт, ошибка: " ++ show a ++ "\n" ++ renderCompactStamp st
                    SolveExceptionCheck i a ->
                        err $ "Антигейт не смог распознать капчу, ошибка: " ++ show a ++ ", id: " ++ show i ++ "\n" ++ renderCompactStamp st
                lg $ "Aborting antigate thread for " ++ renderCompactStamp st
                io $ captchaSend AbortCaptcha
            ,Handler $ \(e::AsyncException) -> do
                lg $ "Antigate thread killed by async " ++ show e ++ " " ++ renderCompactStamp st
            ,Handler $ \e -> do
                io $ captchaSend AbortCaptcha
                io $ errex e
            ]

startAntigateThread :: (OriginStamp, SupplyCaptcha) -> E (ThreadId, (OriginStamp, SupplyCaptcha))
startAntigateThread c@(OriginStamp{..},_) = do
    E{..} <- ask
    writeLog $ "Spawning antigate thread for {" ++ show oProxy ++ "} " ++ renderBoard oBoard
    apikey <- do
        weak <- get wentryantigatekey
        weah <- get wentryantigatehost
        return def {api_host=weah, api_key=weak}
    i <- io $ forkIO $ antigateThread connection c antigateLogQueue apikey
    return (i, c)

addAntigateCaptchas :: [(OriginStamp, SupplyCaptcha)] -> E ()
addAntigateCaptchas [] = writeLog "Added 0 antigate captchas..."
addAntigateCaptchas sps = do
    E{..} <- ask
    modM pendingAntigateCaptchas $ \x -> (x ++) <$> mapM startAntigateThread sps

addAntigateCaptcha :: (OriginStamp, SupplyCaptcha) -> E ()
addAntigateCaptcha sp = addAntigateCaptchas [sp]

filterDead :: E ()
filterDead = do
    E{..} <- ask
    modM pendingAntigateCaptchas $ filterM $ \(t, _) -> io $
        (\ts -> ts /= ThreadDied && ts /= ThreadFinished) <$> threadStatus t

displayLogs :: E ()
displayLogs = do
    E{..} <- ask
    msgs <- untilNothing (io $ atomically $ tryReadTQueue antigateLogQueue)
    forM_ msgs $ \a ->
        case a of
            Left e -> tempError 3 e
            Right l -> writeLog l

filterBlacklist :: [(Board, [BlastProxy])] -> E ()
filterBlacklist blacklist = do
    E{..} <- ask
    pac <- get pendingAntigateCaptchas
    pxs <- get proxies
    let (good, bad) =
            partition (\(_, (OriginStamp{..}, _)) ->
                        (fromMaybe False $ notElem oProxy <$> lookup oBoard blacklist)
                        -- if a board isn't in blacklist, then it must be inactive.
                        && M.member oProxy pxs) pac
    set pendingAntigateCaptchas good
    forM_ bad $ \(t, (st,_)) -> do
        writeLog $ "Killing dead antigate captcha for " ++ renderCompactStamp st
        io $ killThread t

maintainAntigateCaptcha :: [(Board, [BlastProxy])] -> E ()
maintainAntigateCaptcha blacklist = do
    displayLogs
    filterDead
    filterBlacklist blacklist

killAntigateCaptchas :: E [(OriginStamp, SupplyCaptcha)]
killAntigateCaptchas = do
    E{..} <- ask
    oldPac <- get pendingAntigateCaptchas
    pc <- forM oldPac $ \(t, c) -> do
        writeLog "Killing antigate thread"
        io $ killThread t
        return c
    set pendingAntigateCaptchas []
    return pc

killAntigateCaptcha :: E ()
killAntigateCaptcha = do
    E{..} <- ask
    writeLog "Killing antigate captcha"
    void $ killAntigateCaptchas
    displayLogs

deactivateAntigateCaptcha :: E [(OriginStamp, SupplyCaptcha)]
deactivateAntigateCaptcha = do
    E{..} <- ask
    writeLog "Deactivating antigate captcha..."
    pc <- killAntigateCaptchas
    displayLogs
    return pc

antigateCaptchaEnvPart :: Builder -> EnvPart
antigateCaptchaEnvPart b = EP
    (\e c -> do
        wentryantigatekey <- (rec coAntigateKey $ builderGetObject b castToEntry "entryantigatekey") e c
        wentryantigatehost <- (rec coAntigateHost $ builderGetObject b castToEntry "entryantigatehost") e c

        pendingAntigateCaptchas <- newIORef []
        antigateLogQueue <- atomically newTQueue

        let restart = do
                E{..} <- ask
                whenM ((==Antigate) <$> get captchaMode) $ do
                    addAntigateCaptchas =<< deactivateAntigateCaptcha

        void $ on wentryantigatekey entryActivate $ runE e restart
        void $ on wentryantigatehost entryActivate $ runE e restart

        return (wentryantigatekey, wentryantigatehost,
                pendingAntigateCaptchas, antigateLogQueue))
    (\(wak,wah,_,_) c -> do
        cak <- get wak
        cah <- get wah
        return $ c{coAntigateKey=cak
                  ,coAntigateHost=cah
                  })
    (\(weak,weah,pac,alq) e ->
        e{wentryantigatekey=weak
         ,wentryantigatehost=weah
         ,pendingAntigateCaptchas=pac
         ,antigateLogQueue=alq
         })
