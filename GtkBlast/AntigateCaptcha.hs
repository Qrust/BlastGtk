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
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Board
import "blast-it-with-piss" BlastItWithPiss.Blast
import Graphics.UI.Gtk hiding (get, set)
import GHC.Conc
import Control.Concurrent.STM
import Text.Recognition.Antigate
import qualified Data.Map as M

recaptchaCaptchaConf :: CaptchaConf
recaptchaCaptchaConf =
    def {phrase = True
        ,regsense = False
        ,numeric = Nothing
        ,calc = False
        ,min_len = 0
        ,max_len = 0
        ,is_russian = False
        ,max_bid = Nothing
        }

antigateThread :: (OriginStamp, Message) -> TQueue (Either String String) -> String -> IO ()
antigateThread (st, SupplyCaptcha{..}) tq key =
    flip catches hands $ do
        -- FIXME we assume recaptcha
        (cid, str) <- solveCaptcha (3*1000000) (3*1000000) key recaptchaCaptchaConf "recaptcha.jpg" captchaBytes
        lg $ "Sending antigate answer \"" ++ str ++ "\" to " ++ renderCompactStamp st
        captchaSend $ Answer str (handle errex . report cid)
  where lg = atomically . writeTQueue tq . Right
        err = atomically . writeTQueue tq . Left
        report cid nst = do
            lg $ "Reporting bad captcha id " ++ show cid ++ " for " ++ renderCompactStamp nst
            reportBad key cid
        errex (e::SomeException) = err (show e)
        hands =
            [Handler $ \(e::SolveException) -> do
                case e of
                    SolveExceptionUpload a ->
                        err $ "Не удалось загрузить капчу на антигейт, ошибка: " ++ show a ++ "\n" ++ renderCompactStamp st
                    SolveExceptionCheck i a ->
                        err $ "Антигейт не смог распознать капчу, ошибка: " ++ show a ++ ", id: " ++ show i ++ "\n" ++ renderCompactStamp st
                lg $ "Aborting antigate thread for " ++ renderCompactStamp st
                captchaSend AbortCaptcha
            ,Handler $ \(e::AsyncException) -> do
                lg $ "Antigate thread killed by async " ++ show e ++ " " ++ renderCompactStamp st
            ,Handler errex
            ]
antigateThread _ _ _ = error "FIXME Impossible happened, switch from Message, to a dedicated SupplyCaptcha type"

startAntigateThread :: (OriginStamp, Message) -> E (ThreadId, (OriginStamp, Message))
startAntigateThread c@(OriginStamp{..},_) = do
    E{..} <- ask
    writeLog $ "Spawning antigate thread for {" ++ show oProxy ++ "} " ++ renderBoard oBoard
    i <- io . forkIO . antigateThread c antigateLogQueue =<< get wentryantigatekey
    return (i, c)

addAntigateCaptchas :: [(OriginStamp, Message)] -> E ()
addAntigateCaptchas [] = writeLog "Added 0 antigate captchas..."
addAntigateCaptchas sps = do
    E{..} <- ask
    modM pendingAntigateCaptchas $ \x -> (x ++) <$> mapM startAntigateThread sps

addAntigateCaptcha :: (OriginStamp, Message) -> E ()
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

killAntigateCaptchas :: E [(OriginStamp, Message)]
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

deactivateAntigateCaptcha :: E [(OriginStamp, Message)]
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
