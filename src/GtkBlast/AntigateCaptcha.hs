module GtkBlast.AntigateCaptcha
    (addAntigateCaptchas
    ,addAntigateCaptcha
    ,killAntigateCaptcha
    ,deactivateAntigateCaptcha
    ,antigateCaptchaEnvPart
    ,maintainAntigateCaptcha
    ) where
import Import hiding (on, mod)

import GtkBlast.Types
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Conf
import GtkBlast.EnvPart

import BlastItWithPiss

import Text.Recognition.Antigate
import Graphics.UI.Gtk hiding (get, set)

import GHC.Conc
import Data.Time.Clock.POSIX

import Control.Monad.Trans.Resource
import qualified Data.Map as M

antigateThread
    :: Manager
    -> IORef Int
    -> (CaptchaOrigin, CaptchaRequest)
    -> (Either Text Text -> IO ())
    -> ApiKey
    -> IO ()
antigateThread connection captchasSolved' (st, CaptchaRequest{..}) sendLog key =
  runResourceT $ (do
    ut <- io (newIORef 0)
    now <- io getPOSIXTime
    let sconf =
          def {api_counter = counter ut now
              ,api_upload_callback = upcallback ut now}

    (cid, str) <- solveCaptcha sconf key captchaConf captchaFilename
                    captchaBytes connection

    lg $ "Sending antigate answer \"" ++ fromString str ++ "\" to "
        ++ renderCaptchaOrigin st
    io $ captchaSend $ Answer str (handle errex . void . report cid)
    io $ atomicModifyIORef' captchasSolved' $ \a -> (a + 1, ())

    lg $ "Antigate thread finished for " ++ renderCaptchaOrigin st
    ) `catches`
        [Handler $ \(e::SolveException) -> do
            case e of
              SolveExceptionUpload a ->
                err $ "Не удалось загрузить капчу на антигейт, ошибка: "
                 ++ show a ++ "\n" ++ renderCaptchaOrigin st
              SolveExceptionCheck i a ->
                err $ "Антигейт не смог распознать капчу, ошибка: " ++ show a
                    ++ ", id: " ++ show i ++ "\n" ++ renderCaptchaOrigin st
            lg $ "Aborting antigate thread for " ++ renderCaptchaOrigin st
            io $ captchaSend AbortCaptcha
        ,Handler $ \(e::AsyncException) -> do
            lg $ "Antigate thread killed by async " ++ show e ++ " " ++
                renderCaptchaOrigin st
            io $ captchaSend AbortCaptcha
        ,Handler $ \(e::SomeException) -> do
            errex e
            io $ captchaSend AbortCaptcha
        ]
  where
    lg :: MonadIO m => Text -> m ()
    lg  = io . sendLog . Right

    err :: MonadIO m => Text -> m ()
    err = io . sendLog . Left

    errex (e::SomeException) =
        err $ "Antigate exception, errex " ++ renderCaptchaOrigin st
          ++ ": " ++ (show e)

    upcallback uploadtime then' cid = do
        now <- getPOSIXTime
        set uploadtime now
        lg $ "Got captcha id for " ++ renderCaptchaOrigin st ++ ": "
          ++ show cid ++ ". Spent " ++ show (now-then') ++ " on uploading"

    counter _ _ _ 0 = return ()
    counter uploadtime then' phase c = do
        ut <- get uploadtime
        now <- getPOSIXTime
        lg $ "Retry #" ++ show c ++ " of " ++ show phase ++ " for " ++
            renderCaptchaOrigin st ++ ": spent " ++
                (case phase of
                  CheckPhase ->
                    show (now - then') ++ " solving captcha, " ++
                    "with " ++ show (now - ut) ++ " spent on checking"
                  UploadPhase -> show (now - then') ++ " on uploading"
                )
    report cid nst = do
        lg $ "Reporting bad captcha id " ++ show cid ++ " for "
            ++ renderCompactStamp nst
        runResourceT $ reportBad key cid connection

startAntigateThread
    :: (CaptchaOrigin, CaptchaRequest)
    -> E (ThreadId, (CaptchaOrigin, CaptchaRequest))
startAntigateThread c@(st,_) = do
    e@E{
       wentryantigatehost
     , wentryantigatekey
     , connection
     , captchasSolved } <- ask
    writeLog $ "Spawning antigate thread for " ++ renderCaptchaOrigin st

    apikey <- do
        weah <- get wentryantigatehost
        weak <- get wentryantigatekey
        return def {api_host = weah, api_key = weak}

    let asyncReporter = postGUIAsync . runE e . either (tempError 2) writeLog

    i <- io $ forkIO $
        antigateThread connection captchasSolved c asyncReporter apikey
    return (i, c)

addAntigateCaptchas :: [(CaptchaOrigin, CaptchaRequest)] -> E ()
addAntigateCaptchas [] = writeLog "Added 0 antigate captchas..."
addAntigateCaptchas sps = do
    E{ pendingAntigateCaptchas } <- ask

    new <- mapM startAntigateThread sps
    mod pendingAntigateCaptchas (++ new)

addAntigateCaptcha :: (CaptchaOrigin, CaptchaRequest) -> E ()
addAntigateCaptcha sp = addAntigateCaptchas [sp]

filterDead :: E ()
filterDead = do
    E{ pendingAntigateCaptchas } <- ask
    modM pendingAntigateCaptchas $
      filterM $ \(t, _) -> io $
        (\ts -> ts /= ThreadDied && ts /= ThreadFinished) <$> threadStatus t

filterBlacklist :: [(Board, [BlastProxy])] -> E ()
filterBlacklist blacklist = do
    E{ pendingAntigateCaptchas
     , proxies } <- ask

    pac <- get pendingAntigateCaptchas
    pxs <- get proxies

    let (good, bad) =
            (`partition` pac) $ \(_, (est, _)) -> case est of
            AgentCaptcha st ->
                -- board should be present in the blacklist, even with an empty
                -- blacklist, if it's currently under wipe. Therefore we put
                -- @fromMaybe False@, to remove captchas from boards which were
                -- disconnected recently and as such aren't present. [HACK]
                (fromMaybe False $
                    notElem (oProxy st) <$> lookup (oBoard st) blacklist)
                && M.member (oProxy st) pxs
            -- Don't touch presolver captchas
            PresolverCaptcha -> True

    set pendingAntigateCaptchas good

    forM_ bad $ \(t, (st,_)) -> do
        writeLog $ "Killing dead antigate captcha for "
            ++ renderCaptchaOrigin st
        io $ killThread t

maintainAntigateCaptcha :: [(Board, [BlastProxy])] -> E ()
maintainAntigateCaptcha blacklist = do
    filterDead
    filterBlacklist blacklist

killAntigateCaptchas :: E [(CaptchaOrigin, CaptchaRequest)]
killAntigateCaptchas = do
    E{ pendingAntigateCaptchas } <- ask
    oldPac <- get pendingAntigateCaptchas
    pc <- forM oldPac $ \(t, c) -> do
        writeLog "Killing antigate thread"
        io $ killThread t
        return c
    set pendingAntigateCaptchas []
    return pc

killAntigateCaptcha :: E ()
killAntigateCaptcha = do
    writeLog "Killing antigate captcha"
    _ <- killAntigateCaptchas
    return ()

deactivateAntigateCaptcha :: E [(CaptchaOrigin, CaptchaRequest)]
deactivateAntigateCaptcha = do
    writeLog "Deactivating antigate captcha..."
    pc <- killAntigateCaptchas
    return pc

antigateCaptchaEnvPart :: Builder -> EnvPart
antigateCaptchaEnvPart b = EP
    (\e c -> do
        let restart = do
                E{ captchaMode } <- ask
                whenM ((==Antigate) <$> get captchaMode) $ do
                    addAntigateCaptchas =<< deactivateAntigateCaptcha

        wentryantigatekey <- setir (coAntigateKey c)
                        =<< builderGetObject b castToEntry "entryantigatekey"
        wentryantigatehost <- setir (coAntigateHost c)
                        =<< builderGetObject b castToEntry "entryantigatehost"

        pendingAntigateCaptchas <- newIORef []

        void $ on wentryantigatekey entryActivate $ runE e restart
        void $ on wentryantigatehost entryActivate $ runE e restart

        return (wentryantigatekey, wentryantigatehost,
                pendingAntigateCaptchas))
    (\(wak,wah,_) c -> do
        cak <- get wak
        cah <- get wah
        return $ c{coAntigateKey=cak
                  ,coAntigateHost=cah
                  })
    (\(weak,weah,pac) e ->
        e{wentryantigatekey=weak
         ,wentryantigatehost=weah
         ,pendingAntigateCaptchas=pac
         })
