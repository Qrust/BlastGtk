module GtkBlast.AntigateCaptcha
    (addAntigateCaptchas
    ,addAntigateCaptcha
    ,killAntigateCaptcha
    ,deactivateAntigateCaptcha
    ,antigateCaptchaEnvPart
    ,maintainAntigateCaptcha
    ) where
import Import hiding (on, mod)
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Conf
import GtkBlast.EnvPart
import BlastItWithPiss
import BlastItWithPiss.Board
import BlastItWithPiss.Blast
import Graphics.UI.Gtk hiding (get, set)
import GHC.Conc
import Control.Concurrent.STM
import Text.Recognition.Antigate
import qualified Data.Map as M
import Control.Monad.Trans.Resource

antigateThread :: Manager -> (OriginStamp, SupplyCaptcha) -> TQueue (Either String String) -> String -> IO ()
antigateThread connection (st, SupplyCaptcha{..}) tq key =
    runResourceT $ flip catches hands $ do
        -- FIXME we assume recaptcha
        (cid, str) <- solveCaptcha (3*1000000) (3*1000000) key captchaConf "recaptcha.jpg" captchaBytes connection
        lg $ "Sending antigate answer \"" ++ str ++ "\" to " ++ renderCompactStamp st
        io $ captchaSend $ Answer str (handle errex . report cid)
        lg $ "Antigate thread finished for " ++ renderCompactStamp st
  where lg :: MonadIO m => String -> m ()
        lg = io . atomically . writeTQueue tq . Right
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
    i <- io . forkIO . antigateThread connection c antigateLogQueue =<< get wentryantigatekey
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

        pendingAntigateCaptchas <- newIORef []
        antigateLogQueue <- atomically newTQueue

        return (wentryantigatekey, pendingAntigateCaptchas, antigateLogQueue))
    (\(v,_,_) c -> get v ? \a -> c{coAntigateKey=a})
    (\(weak,pac,alq) e -> e{wentryantigatekey=weak
                           ,pendingAntigateCaptchas=pac
                           ,antigateLogQueue=alq
                           })
