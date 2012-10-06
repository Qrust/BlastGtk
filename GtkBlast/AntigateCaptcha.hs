{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Graphics.UI.Gtk hiding (get, set)
import GHC.Conc
import Control.Concurrent.STM
import Text.Recognition.Antigate

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
antigateThread (OriginStamp{..}, SupplyCaptcha{..}) tq key =
    flip catches hands $ do
            -- FIXME we assume recaptcha
        (_, str) <- solveCaptcha (3*1000000) (3*1000000) key recaptchaCaptchaConf "recaptcha.jpg" captchaBytes
        lg $ "Sending antigate answer \"" ++ str ++ "\" to {" ++ show oProxy ++"} " ++ renderBoard oBoard
        captchaSend $ Answer str
  where lg = atomically . writeTQueue tq . Right
        err = atomically . writeTQueue tq . Left
        hands =
            [Handler $ \(e::SolveException) -> do
                case e of
                    SolveExceptionUpload a ->
                        err $ "Не удалось загрузить капчу на антигейт, ошибка: " ++ show a ++ "\n" ++ "{" ++ show oProxy ++ "}" ++ renderBoard oBoard
                    SolveExceptionCheck i a ->
                        err $ "Антигейт не смог распознать капчу, ошибка: " ++ show a ++ ", id: " ++ show i ++ "\n" ++ "{" ++ show oProxy ++ "}" ++ renderBoard oBoard
                lg $ "Aborting antigate thread for {" ++ show oProxy ++ "} " ++ renderBoard oBoard
                captchaSend AbortCaptcha
            ]

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

maintainAntigateCaptcha :: E ()
maintainAntigateCaptcha = do
    filterDead
    displayLogs

killAntigateCaptcha :: E ()
killAntigateCaptcha = do
    E{..} <- ask
    writeLog "Killing antigate captcha"
    oldPac <- get pendingAntigateCaptchas
    io $ forM_ oldPac $ killThread . fst
    set pendingAntigateCaptchas []
    displayLogs

deactivateAntigateCaptcha :: E [(OriginStamp, Message)]
deactivateAntigateCaptcha = do
    E{..} <- ask
    writeLog "Deactivating antigate captcha..."
    oldPac <- get pendingAntigateCaptchas
    pc <- forM oldPac $ \(t, c) -> do
        writeLog "Killing antigate thread"
        io $ killThread t
        return c
    displayLogs
    filterDead
    return pc

antigateCaptchaEnvPart :: Builder -> EnvPart
antigateCaptchaEnvPart b = EP
    (\e c -> do
        wentryantigatekey <- (rec coAntigateKey $ builderGetObject b castToEntry "entryantigatekey") e c

        pendingAntigateCaptchas <- newIORef []
        antigateLogQueue <- atomically $ newTQueue

        return (wentryantigatekey, pendingAntigateCaptchas, antigateLogQueue))
    (\(v,_,_) c -> get v ? \a -> c{coAntigateKey=a})
    (\(weak,pac,alq) e -> e{wentryantigatekey=weak
                           ,pendingAntigateCaptchas=pac
                           ,antigateLogQueue=alq
                           })
