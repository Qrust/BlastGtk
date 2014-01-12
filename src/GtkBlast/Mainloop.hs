-- | What does this module do?
--
-- * creates and kills wipe threads. → move to BlastItWithPiss
-- * manages perboard MuSettings → move to BlastItWithPiss
-- * starts and stops wipe → move to BlastItWithPiss
-- * sets up board panel widget
-- * sets up board settings widget
-- * sets up wipe button widget
-- * sets up main loop
-- * starts application
--
-- badly needs a rewrite
-- TODO Instead of 'maintaining' launch a overseer thread for every board, use
--      ResourceT to manage wipeAgents

module GtkBlast.Mainloop
    (wipebuttonEnvPart
    ,presolveCaptchaEnvPart
    ,boardUnitsEnvPart
    ,setMainLoop
    ) where
import Import hiding (on, mod)
import qualified Data.Function as F (on)
import qualified Numeric as NumericPrint (showFFloat)

import Paths_blast_it_with_piss

import GtkBlast.Worker (BoardUnit(..), WipeUnit(..))
import GtkBlast.Achievement
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.Log
import GtkBlast.EnvPart
import GtkBlast.Captcha
import GtkBlast.Proxy
import GtkBlast.BoardSettingsGuiXML
import GtkBlast.Types

import BlastItWithPiss
import BlastItWithPiss.Choice (Mode(SagePopular, BumpUnpopular))
import BlastItWithPiss.Parsing
import BlastItWithPiss.Blast
import BlastItWithPiss.Board

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.FinalizerTVar
import Control.Monad.Fix (mfix)

import qualified Data.Text as T
import qualified Data.Map as M

import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as G (set)
import System.Glib.GObject (GObjectClass(unsafeCastGObject))

import GHC.Conc hiding (forkIO)
import qualified Control.Concurrent.Thread.Group as ThreadGroup

-- * ReactLoop

-- (can't be separated right now, 'setBanned' needs 'killWipeUnit')

asyncWriteLog :: Env -> (Text -> IO ())
asyncWriteLog e = postGUIAsync . runE e . writeLog

asyncAddCaptcha :: Env -> (CaptchaOrigin -> CaptchaRequest -> IO ())
asyncAddCaptcha e = ((postGUIAsync . runE e) .) . addCaptcha'
  where
    addCaptcha' st c = do
        ws <- io $ readTVarIO (wipeStarted e)
        if ws
          then addCaptcha (st, c)
          else do
            io $ captchaSend c AbortCaptcha
            writeLog $ renderCaptchaOrigin st
             ++ ": Попытался дать капчу на решение когда вайп уже кончился"

asyncReactToMessage :: Env -> (OutMessage -> IO ())
asyncReactToMessage e = postGUIAsync . runE e . reactToMessage

reactToMessage :: OutMessage -> E ()
reactToMessage (OutMessage st m) = do
    let board = oBoard st
        proxy = oProxy st

    case m of
      LogMessage msg -> writeLog $ renderFullStamp st ++ ": " ++ msg
      OutcomeMessage o -> do
        case o of
          SuccessLongPost _ ->
            writeLog $ renderFullStamp st ++ ": SuccessLongPost"
          _ -> writeLog $ renderFullStamp st ++ ": " ++ show o
        case o of
          Success -> addPost
          SuccessLongPost _ -> addPost

          OtherError x -> tempError 4 $ stamp $ "" ++ show x
          InternalError x -> tempError 4 $ stamp $ "" ++ show x

          NeedCaptcha -> writeLog $ stamp $ "NeedCaptcha"
          WrongCaptcha -> writeLog $ stamp $ "WrongCaptcha"

          Wordfilter -> tempError 3 $ stamp $ "Не удалось обойти вордфильтр"
          ThreadDoesNotExist -> tempError 5 $ stamp $ "Тред удалён"
          SameMessage -> tempError 2 $ stamp $ "Вы уже постили это сообщение"
          SameImage -> tempError 2 $ stamp $ "Этот файл уже загружен"
          TooFastPost -> writeLog $ stamp $
                "Вы постите слишком часто, умерьте пыл"
          TooFastThread -> tempError 3 $ stamp $
                "Вы создаете треды слишком часто"
          LongPost -> tempError 1 $ stamp $ "Запостил слишком длинный пост"
          EmptyPost -> tempError 2 $ stamp $
                "Вы ничего не написали в сообщении и не прикрепили картинку"
          CorruptedImage -> tempError 2 $ stamp $
                "Запостил поврежденное изображение"

          PostRejected -> tempError 1 $ stamp $ "PostRejected"
          Five'o'ThreeError -> tempError 1 $ stamp $ "Ошибка 503"

          Banned x -> do
            banMessage 5 $
                "Забанен " ++ renderCompactStamp st
             ++ " Причина: " ++ show x
             ++ "\nВозможно стоит начать вайпать /d/"
            setBanned board proxy
          RecaptchaBan -> do
            banMessage 7 $ stamp $ "Забанен рекапчой, охуеть."
            setBanned board proxy
          CloudflareCaptcha -> do
            banMessage 2 $ stamp $
                "Если эта ошибка появляется то это баг, сообщите нам об этом "
             ++ "(Не могу обойти капчу клаудфлера во время постинга "
             ++ "https://github.com/exbb2/BlastItWithPiss/issues/4)"
            setBanned board proxy
          CloudflareBan -> do
            banMessage 2 $ stamp $ "Бан по клаудфлеру"
            setBanned board proxy
          Four'o'FourBan -> do
            banMessage 2 $ stamp $ "Бан по 404"
            setDead board proxy
          Four'o'ThreeBan -> do
            banMessage 2 $ stamp $ "Бан по 403"
            setDead board proxy
          UnknownError i ->
            tempError 4 $ stamp $
                "Неизвестная ошибка, что-то пошло не так, http status code: "
             ++ show i
--      NoPastas -> do
--        writeLog (show s)
--        tempError 3 "Невозможно прочитать пасты, постим нихуя"
--      NoImages -> do
--        writeLog (show s)
--        tempError 3 "Невозможно прочитать пикчи, постим капчу"
  where
    stamp msg = renderCompactStamp st ++ ": " ++ msg

setBanned :: Board -> BlastProxy -> E ()
setBanned board proxy = do
    writeLog $ "setBanned: " ++ renderBoard board ++ " {" ++ show proxy ++ "}"
    bus <- asks boardUnits
    whenJust (find ((==board) . buBoard) bus) $ \BoardUnit{..} -> do
        bwus <- get buWipeUnits
        whenJust (find ((==proxy) . wuProxy) bwus) $ \wu -> do
            writeLog $
                "Banning " ++ renderBoard board ++ " {" ++ show proxy ++ "}"
            killWipeUnit buBoard wu
            mod buWipeUnits $ delete wu
            mod buBanned (wuProxy wu :)

setDead :: Board -> BlastProxy -> E ()
setDead board proxy = do
    writeLog $ "setDead: " ++ renderBoard board ++ " {" ++ show proxy ++ "}"
    bus <- asks boardUnits
    whenJust (find ((==board) . buBoard) bus) $ \BoardUnit{..} -> do
        bwus <- get buWipeUnits
        whenJust (find ((==proxy) . wuProxy) bwus) $ \wu -> do
            writeLog $
                "Banning " ++ renderBoard board ++ " {" ++ show proxy ++ "}"
            killWipeUnit buBoard wu
            mod buWipeUnits $ delete wu
            mod buDead (wuProxy wu :)

addPost :: E ()
addPost = do
    E{ postCount
     , firstPostStats } <- ask

    pc <- get postCount
    fps <- get firstPostStats
    case fps of
      Left n -> when (n == pc) $ do
            now <- io getCurrentTime
            set firstPostStats (Right (now, pc + 1))
      _ -> return ()

    set postCount (pc + 1)
    writeLog =<< ("Updated post count: " ++) . show <$> get postCount

-- * CaptchaPresolving

data CaptchaServerType
    = Primitive
    | Presolving
  deriving (Eq, Show)

captchaServerType :: Bool -> CaptchaServerType
captchaServerType True  = Presolving
captchaServerType False = Primitive

setCaptchaServer :: CaptchaServerType -> E ()
setCaptchaServer Primitive = do
    e@E{
       shS
     , captchaCache
     , accessPresolverState} <- ask

    set accessPresolverState Nothing

    pcs <- io $ primitiveCaptchaServer
        captchaCache
        (tstartsignal shS)

    io $ atomically $ writeFinalizerTVar (tcaptchaserver shS) pcs
        (asyncWriteLog e $ "primitive captcha server shutting down.")
setCaptchaServer Presolving = do
    e@E{
       shS
     , captchaCache
     , captchaKeysStore
     , boardUnits
     , accessPresolverState } <- ask

    io $ atomically $
        writeTVar (tstartsignal shS) False

    (cs, f, gps) <- io $ do
        let
          -- FIXME KLUDGE duplicates code in BlastItWithPiss.hs
          _kludge_getPostTimeout BoardUnit{..} = do
              x1 <- fmap realToFrac <$> readTVar (mposttimeout buMuSettings)
              case x1 of
                Just _x -> return _x
                Nothing -> do
                  x2 <- fmap realToFrac <$> readTVar (tposttimeout shS)
                  case x2 of
                    Just _x -> return _x
                    Nothing -> return $ ssachPostTimeout buBoard
          collectedBoardUnitData = flip map boardUnits $ \bu@BoardUnit{..} ->
            (buBoard, _mMutAgentCount buMuSettings, _kludge_getPostTimeout bu)

        presolvingCaptchaServer
            captchaCache
            captchaKeysStore
            (asyncWriteLog e)
            collectedBoardUnitData
            (tstartsignal shS)

    set accessPresolverState $ Just gps

    io $ atomically $ writeFinalizerTVar (tcaptchaserver shS) cs f

-- * MaintenanceLoop

updWipeMessage :: E ()
updWipeMessage = do
    E{ wipeStarted
     , wipeStats
     , postCount
     , wlabelstats
     , accessPresolverState
     , firstPostStats
     , captchasSolved } <- ask

    clearMessage

    whenM (io $ readTVarIO wipeStarted) $ do
        pc <- get postCount

        (active, banned, dead) <- get wipeStats

        let ach = getAchievementString pc

        prs <- do
            _getPS <- get accessPresolverState
            case _getPS of
              Nothing -> return ""
              Just getPS -> do
                PresolverState{..} <- io $ atomically getPS
                return $ "\nСбор капчи: нужно капчи "
                 ++ show (round presolverMaxLimit :: Int)
                 ++ ", в запасе: " ++ show presolverStored ++ ", в процессе: "
                 ++ show presolverEnRoute ++ ". Ключей: "
                 ++ show presolverKeysIn

        spd <- do
            fps <- get firstPostStats
            case fps of
              Right (starttime, startpost) -> do
                now <- io getCurrentTime
                let wipetime_min = realToFrac $ diffUTCTime now starttime / 60
                    wipeposts = realToFrac $ pc - startpost
                return $ if wipetime_min == 0
                  then ""
                  else " (" ++ fromString (NumericPrint.showFFloat (Just 3)
                        (wipeposts / wipetime_min :: Double) " п/мин )")
              _ -> return ""

        cpc <- do
            capCount <- get captchasSolved
            return $ if capCount == 0
                then ""
                else " / Капч решено: " ++ show capCount

        io $ labelSetText wlabelstats $ T.unpack $
               "Постов: " ++ show pc ++ spd ++ " / "
            ++ "Активно: " ++ show active
            ++ " / Забанено: " ++ show banned
            ++ (if dead > 0 then " / Наебнулось: " ++ show dead else "")
            ++ (if T.null ach then "" else "\n" ++ ach)
            ++ prs ++ cpc

killWipeUnit :: Board -> WipeUnit -> E ()
killWipeUnit board WipeUnit{..} = do
    writeLog $ "Killing thread for " ++ renderBoard board
     ++ " {" ++ show wuProxy ++ "}"
#ifdef mingw32_HOST_OS
    _ <- io $ forkIO $ killThread wuThreadId
#else
    io $ killThread wuThreadId
#endif
    writeLog $
        "Killed thread for " ++ renderBoard board ++ " {" ++ show wuProxy ++ "}"

killBoardUnitWipeUnits :: BoardUnit -> E ()
killBoardUnitWipeUnits BoardUnit{..} = do
    mapM_ (killWipeUnit buBoard) =<< get buWipeUnits
    set buWipeUnits []

cleanBoardUnitBadRecord :: BoardUnit -> E ()
cleanBoardUnitBadRecord BoardUnit{..} = do
    {- -- it doesn't make sense. proxies won't ever get unbanned
    unlessM (null <$> get buBanned) $ do
      writeLog $
        "Cleaning board unit " ++ renderBoard buBoard ++ " banned proxy records"
      set buBanned []
    -}
    unlessM (null <$> get buDead) $ do
        writeLog $
          "Cleaning board unit " ++ renderBoard buBoard ++ " dead proxy records"
        set buDead []

killBoardUnit :: BoardUnit -> E ()
killBoardUnit bu = do
    killBoardUnitWipeUnits bu
    cleanBoardUnitBadRecord bu

newWipeUnit
    :: Board
    -> BlastProxy
    -> MuSettings
    -> ProxySettings
    -> E WipeUnit
newWipeUnit board bproxy muSettings proxySettings = do
    e@E{
      threadGroup
    , shS
    , connection } <- ask

    writeLog $
        "Spawning new thread for " ++
        renderBoard board ++ " {" ++
        show bproxy ++ "}"
#ifdef mingw32_HOST_OS
    threadid <- fmap fst $ io $ ThreadGroup.forkOS threadGroup $ do
#else
    threadid <- fmap fst $ io $ ThreadGroup.forkIO threadGroup $ do
#endif
        entryPoint connection bproxy board shS muSettings proxySettings
            (asyncReactToMessage e) (asyncAddCaptcha e)
    return $ WipeUnit bproxy threadid

-- | Two jobs:
-- * Remove wipe units belonging to proxies in supplied blacklist
-- * For proxies which don't have a wipeunit associated, create new wipe units
--   with supplied boardSettings
regenerateExcluding :: Board -> [BlastProxy] -> MuSettings -> E [WipeUnit]
regenerateExcluding board excludeThese muSettings = do
    E{ proxies } <- ask
    prx <- M.assocs <$> get proxies
    forMaybeM prx $ \(bproxy, prSettings) ->
        if bproxy `elem` excludeThese
          then
            return Nothing
          else do
            Just <$> newWipeUnit board bproxy muSettings prSettings

-- | Check whether the thread associated with WipeUnit is still alive, and
--   whether there are reasons to kill it.
maintainWipeUnit
    :: BoardUnit
    -- | Wipe is activated for this board? If not — kill wipe unit
    -> Bool
    -- | Wipe is activated globally? If not — kill wipe unit
    -> Bool
    -- | Unit in question
    -> WipeUnit
    -- | Nothing    -> WipeUnit is no longer needed, and has been killed off.
    --   Left proxy -> WipeUnit died abruptly, blacklist the proxy as erroneous.
    --   Right wu   -> Nothing bad happened, carry on.
    -> E (Maybe (Either BlastProxy WipeUnit))
maintainWipeUnit BoardUnit{..} isActive hasWipeStarted w@WipeUnit{..} = do
    E{ proxies } <- ask

    st <- io $ threadStatus wuThreadId
    pxs <- get proxies
    if st == ThreadDied || st == ThreadFinished
      then do
        writeLog $
            "blastgtk: Thread for {" ++ show wuProxy
            ++ "} " ++ renderBoard buBoard ++
            " died. Removing"
        return $ Just $ Left wuProxy
      else
        if not isActive || not hasWipeStarted || M.notMember wuProxy pxs
          then do
            writeLog $
                "Removing unneded {" ++ show wuProxy
                ++ "} " ++ renderBoard buBoard
            killWipeUnit buBoard w
            return Nothing
          else
            return $ Just $ Right w

-- | Calls 'maintainWipeUnit' on all wipeUnits in boardUnit
--   then, if the wipe is on globally and for board, calls 'regenerateExcluding'
--   to introduce new wipe agents
maintainBoardUnit
    :: (Int, [(Board, [BlastProxy])], [(Board, [BlastProxy])])
    -- ^ (Active units
    -- , list of banned proxies per board
    -- , list of dead proxies per board)
    -> BoardUnit
    -> E (Int, [(Board, [BlastProxy])], [(Board, [BlastProxy])])
maintainBoardUnit (!activecount, !pbanned, !pdead) bu@BoardUnit{..} = do
    E{ wipeStarted } <- ask

    wipeHadStarted <- io $ readTVarIO wipeStarted
    thisBoardIsActive <- get buWidget

    wipeUnits' <- get buWipeUnits

    (newdead, old) <-
        fmap partitionEithers $
          forMaybeM wipeUnits' $ \wu ->
            maintainWipeUnit bu thisBoardIsActive wipeHadStarted wu

    mod buDead (newdead++)

    banned <- get buBanned
    dead <- get buDead

    new <- if thisBoardIsActive && wipeHadStarted
          then do
            let blacklist = banned ++ dead ++ map wuProxy old
            regenerateExcluding buBoard blacklist buMuSettings
          else
            return []
    let allwus = old ++ new

    set buWipeUnits allwus

    if thisBoardIsActive
      then return $
        (activecount + length allwus
        ,(buBoard, banned) : pbanned
        ,(buBoard, dead) : pdead
        )
      else return $
        (activecount, pbanned, pdead)

-- | Several things:
--
--  Input:

--  * Folds 'maintainBoardUnit' over boards. — Creates and destroy wipe units,
--    collects dead and banned proxies.
--  * Updates 'wipeStats', with counts of active, banned and dead agents.
--  * If activeAgents = 0, calls 'killWipe'
--  * Calls 'updWipeMessage'
--
--  OH DOG
maintainBoardUnits :: E [(Board, [BlastProxy])]
maintainBoardUnits = do
    E{ boardUnits
     , proxies
     , wipeStats } <- ask
    (active, banned, dead) <- foldM maintainBoardUnit (0,[],[]) boardUnits

    let !bannedCount = length $ concatMap snd banned
        !deadCount = length $ concatMap snd dead
    set wipeStats (active, bannedCount, deadCount)

    updWipeMessage

    when (active == 0) $ do
        killWipe

        noProxies <- M.null <$> get proxies
        if noProxies
          then
            redMessage
                "Нет проксей, выберите прокси для вайпа или вайпайте без прокси"
          else
            if bannedCount > 0 || deadCount > 0
              then
                -- KLUDGE
                --  Don't display message, as it obscures ban/error reasons
                --  in error label
                return ()

                -- uncAnnoyMessage
                --    "Все треды забанены или наебнулись."
                -- ++ Выберите другие доски для вайпа или "
                -- ++ "найдите прокси не являющиеся калом ёбаным."
              else
                -- Nothing banned or dead, must be that there wasn't a wipe in
                -- the first place.
                redMessage "Выберите доски для вайпа"

    return (banned ++ dead)

startWipe :: E ()
startWipe = do
    E{ wipeStarted
     , wbuttonwipe
     , wprogresswipe
     , wcheckpresolvecaptcha
     , firstPostStats
     , postCount } <- ask


    writeLog "Starting wipe..."
    uncMessage "Заряжаем пушки..."
    io $ buttonSetLabel wbuttonwipe "Прекратить _Вайп"
    io $ progressBarPulse wprogresswipe

    io $ atomically $ writeTVar wipeStarted True
    setCaptchaServer . captchaServerType =<< get wcheckpresolvecaptcha
    set firstPostStats . Left =<< get postCount

-- | This is called when there are zero active agents OR the user stops wipe
killWipe :: E ()
killWipe = do
    E{ wipeStarted
     , boardUnits
     , threadGroup
     , connection
     , wbuttonwipe
     , wprogresswipe } <- ask

    io $ atomically $ writeTVar wipeStarted False
    -- HACK? destroy captcha server
    setCaptchaServer Primitive

    writeLog "Stopping wipe..."

    -- close all connections
    io $ closeManager connection

    mapM_ killBoardUnit boardUnits
  -- io is uninterruptible on dos, so workers might not even be able to die
#ifndef mingw32_HOST_OS
    io $ ThreadGroup.wait threadGroup
#endif

    -- once all generators are dead, remove captcha
    killAllCaptcha

    io $ buttonSetLabel wbuttonwipe "Начать _Вайп"
    io $ progressBarSetFraction wprogresswipe 0
    --uncMessage "Вайп ещё не начат"

mainloop :: E ()
mainloop = do
    E{ wipeStarted } <- ask

    whenM (io $ readTVarIO wipeStarted) $ do
        regenerateProxies
        bus <- maintainBoardUnits
        maintainCaptcha bus

setMainLoop :: Default Conf => Env -> FilePath -> (Conf -> IO Conf) -> IO ()
setMainLoop env configfile setConf = do
    runE env $ writeLog "Setting timeouts."
    void $ timeoutAddFull (do
        whenM (readTVarIO $ wipeStarted env) $ do
            progressBarPulse $ wprogresswipe env
        return True) priorityLow 30
    void $ timeoutAddFull (do
        runE env mainloop
        return True) priorityDefaultIdle 50 --kiloseconds, 20 fps.
    void $ onDestroy (window env) $ runE env $ do
        writeLog "Closing gtkblast"
        conf <- io $ setConf
            def { coFirstLaunch = False
                , coLastVersion = GtkBlastVersion version
                }
        writeConfig configfile conf
        writeLog "Shutting down GUI"
        io $ mainQuit
        writeLog "GUI shut down."
    runE env $ writeLog "Launching..."

showBoardSettings :: Board -> E ()
showBoardSettings board = do
    e <- ask
    case find ((==board) . buBoard) $ boardUnits e of
      Nothing -> do
        writeLog $ "showBoardSettings: ERROR couldn't find boardunit for "
            ++ renderBoard board
      Just BoardUnit{buMuSettings = MuSettings{..}} -> io $ do
        b <- builderNew
        builderAddFromString b $ boardSettingsGuiXML board
        window <- builderGetObject b castToWindow "window1"

        mtrd <- readTVarIO mthread
        mmod <- readTVarIO mmode
        mposttm <- readTVarIO mposttimeout
        mtrdtm <- readTVarIO mthreadtimeout

        walignmentthread <-
            builderGetObject b castToAlignment "alignmentthread"
        wcheckwipethread <-
            builderGetObject b castToCheckButton "checkwipethread"
        set wcheckwipethread $ isJust mtrd && isJust mmod
        widgetSetSensitive walignmentthread $ isJust mtrd && isJust mmod

        wspinthreadnum <- builderGetObject b castToSpinButton "spinthreadnum"
        whenJust mtrd $ set wspinthreadnum . fromIntegral

        wchecksage <- builderGetObject b castToCheckButton "checksage"
        whenJust mmod $ \mode -> do
            if mode /= SagePopular && mode /= BumpUnpopular
              then do
                runE e $ tempError 3
                    "TERRIBLE! showBoardSettings ERROR: Unknown mode."
                toggleButtonSetInconsistent wchecksage True
              else do
                set wchecksage (mode == SagePopular)

        wcheckposttimeout <-
            builderGetObject b castToCheckButton "checkposttimeout"
        set wcheckposttimeout $ isJust mposttm

        wspinposttimeout <-
            builderGetObject b castToSpinButton "spinposttimeout"
        set wspinposttimeout $ fromMaybe (ssachPostTimeout board) mposttm

        wcheckthreadtimeout <-
            builderGetObject b castToCheckButton "checkthreadtimeout"
        set wcheckthreadtimeout $ isJust mtrdtm

        wspinthreadtimeout <-
            builderGetObject b castToSpinButton "spinthreadtimeout"
        set wspinthreadtimeout $ fromMaybe (ssachThreadTimeout board) mtrdtm

        wbuttonapply <- builderGetObject b castToButton "buttonapply"
        wbuttoncancel <- builderGetObject b castToButton "buttoncancel"
        wbuttonok <- builderGetObject b castToButton "buttonok"

        void $ on wcheckwipethread buttonActivated $ do
            widgetSetSensitive walignmentthread =<< get wcheckwipethread

        void $ on wbuttoncancel buttonActivated $ do
            widgetDestroy window

        void $ on wbuttonok buttonActivated $ do
            buttonClicked wbuttonapply
            widgetDestroy window

        let ifMJust c t = ifM c (Just <$> t) (return Nothing)

        void $ on wbuttonapply buttonActivated $ do
            nmthread <- ifMJust (get wcheckwipethread)
                            (spinButtonGetValueAsInt wspinthreadnum)
            atomically $ writeTVar mthread nmthread
            runE e $ writeLog $
                renderBoard board ++ ": new thread: " ++ show nmthread

            nmmode <- ifMJust (get wcheckwipethread)
                            (ifM (get wchecksage)
                                (return SagePopular)
                                (return BumpUnpopular))
            atomically $ writeTVar mmode nmmode
            runE e $ writeLog $
                renderBoard board ++ ": new mode: " ++ show nmmode

            nmposttimeout <- ifMJust (get wcheckposttimeout)
                            (get wspinposttimeout)
            atomically $ writeTVar mposttimeout nmposttimeout
            runE e $ writeLog $
                renderBoard board ++ ": new post timeout: "
                                  ++ show nmposttimeout

            nmthreadtimeout <- ifMJust (get wcheckthreadtimeout)
                            (get wspinthreadtimeout)
            atomically $ writeTVar mthreadtimeout nmthreadtimeout
            runE e $ writeLog $
                renderBoard board ++ ": new thread timeout: "
                                  ++ show nmthreadtimeout
            -- TODO Config read/write post timeout & thread timeout

        widgetShowAll window

presolveCaptchaEnvPart :: Builder -> EnvPart
presolveCaptchaEnvPart b = EP
    (\e c -> do
        -- checker
        wcheckpresolvecaptcha <- setir (coPresolveCaptcha c) =<<
            builderGetObject b castToCheckButton "checkpresolvecaptcha"

        _ <- on wcheckpresolvecaptcha toggled $
            whenM (readTVarIO $ wipeStarted e) $
              runE e $ setCaptchaServer =<<
                    (captchaServerType <$> get wcheckpresolvecaptcha)

        -- skip button
        wskippresolving <-
            builderGetObject b castToButton "buttonskippresolving"

        widgetSetNoShowAll wskippresolving True
        widgetHide wskippresolving

        _ <- on wskippresolving buttonActivated $ do
            old <- atomically $ do
                old <- readTVar $ tstartsignal $ shS e
                old <$ unless (old) (
                    writeTVar (tstartsignal $ shS e) True)

            unless old $ runE e $
                writeLog "SKIPPING PRESOLVING; START WIPE NOWWWWWW"

        _ <- forkIO $ (`fix` False) (\recurse visible -> do
            showbutton <- atomically $ do
                actuallyStarted <- readTVar $ tstartsignal $ shS e
                presumablyStarted <- readTVar $ wipeStarted e
                if presumablyStarted && not actuallyStarted && not visible
                  then return True
                  else if (not presumablyStarted || actuallyStarted) && visible
                    then return False
                    else retry

            postGUISync $ if showbutton
              then widgetShow wskippresolving
              else widgetHide wskippresolving

            recurse showbutton
            )

        ccache <- newCaptchaCache
        ckstore <- newCaptchaKeysStore
        prs <- newIORef Nothing
        return (wcheckpresolvecaptcha, ccache, ckstore, prs)
    )
    (\(wcpc, _, _, _) c -> do
        pc <- get wcpc
        return c{coPresolveCaptcha = pc})
    (\(wcpc, cc, cks, prs) e ->
        e{wcheckpresolvecaptcha = wcpc
         ,captchaCache = cc
         ,captchaKeysStore = cks
         ,accessPresolverState = prs})

boardUnitsEnvPart :: Builder -> EnvPart
boardUnitsEnvPart b = EP
    (\e c -> do

        currentSort <- newIORef $
            (M.fromList (coBoardSpeedData c)
                `M.union` M.fromList ssachBoardsSortedByPostRate)
                    `M.union` M.fromList (zip allSsachBoards (repeat 0))

        wvboxboards <- builderGetObject b castToVBox "vbox-boards"

        boardUnits <-
          forM allSsachBoards $ \board -> do
            whb <- hBoxNew False 0

            wc <- checkButtonNewWithLabel $ renderBoard board
            when (board `elem` coActiveBoards c) $ toggleButtonSetActive wc True

            -- boardUnitsEnvPart needs access to PerBoardSettings (MuSettings)
            wb <- buttonNew
            containerAdd wb =<< imageNewFromStock stockEdit IconSizeMenu
            buttonSetRelief wb ReliefNone
            G.set wb [widgetTooltipText
                        := Just ("Настроить вайп " ++ renderBoard board)]
            void $ on wb buttonActivated $ do
                runE e $ showBoardSettings board

            boxPackStart whb wc PackGrow 0
            boxPackStart whb wb PackNatural 0

            boxPackStart wvboxboards whb PackNatural 0

            mus <- defMuS
            BoardUnit
                board
                wc
                <$> newIORef []
                <*> newIORef []
                <*> newIORef []
                <*> pure mus

        wbuttonselectall <- builderGetObject b castToButton "buttonselectall"
        wbuttonselectnone <- builderGetObject b castToButton "buttonselectnone"
        wchecksort <- setir (coSortingByAlphabet c)
                        =<< builderGetObject b castToCheckButton "checksort"

        void $ on wbuttonselectall buttonActivated $ do
            forM_ boardUnits $
                (`toggleButtonSetActive` True) . buWidget

        void $ on wbuttonselectnone buttonActivated $ do
            forM_ boardUnits $
                (`toggleButtonSetActive` False) . buWidget

        let
          resortBoards = do
            currentPositions <- get currentSort

            forM_ boardUnits $ \BoardUnit{buBoard, buWidget} ->
                whenJust (M.lookup buBoard currentPositions) $ \sp ->
                    G.set buWidget
                        [widgetTooltipText := Just (show sp ++ " п./час")]

            bySpeed <- get wchecksort

            reorderedBoards <-
                if bySpeed
                  then
                    -- sort boards by Enum value
                    return $ sortBy (compare `F.on` buBoard) boardUnits
                  else do
                    -- sort boards by speed in 'currentSort'
                    let
                      getSpeed BoardUnit{buBoard} =
                            M.lookup buBoard currentPositions
                        -- or in 'ssachBoardsSortedByPostRate', if something
                        -- is missing
                        <|> lookup buBoard ssachBoardsSortedByPostRate
                        & fromMaybe 0

                    return $
                        sortBy (compare `F.on` negate . getSpeed) boardUnits

            foldM_ (\ !i bu -> do
                maybeParent <- widgetGetParent (buWidget bu)
                whenJust maybeParent $ \w ->
                    boxReorderChild wvboxboards w i
                return $ i+1
                ) 0 reorderedBoards

        resortBoards

        void $ on wchecksort buttonActivated resortBoards

        wspinnersort :: Spinner <-
            -- no cast for spinner? For fuck's sake, gtk2hs...
            builderGetObject b (unsafeCastGObject . castToGObject)
                "spinnersort"
        wbuttonupdatespeed <-
            builderGetObject b castToButton "buttonupdatespeed"

        widgetSetNoShowAll wspinnersort True
        widgetSetNoShowAll wbuttonupdatespeed True

        widgetHide wspinnersort

        weventboxsort <- builderGetObject b castToEventBox "eventboxsort"

        _ <- on wbuttonupdatespeed buttonActivated $ do
          ~_ <- mfix $ \ ~tid -> do
            connId <- do
                widgetShow wspinnersort
                widgetHide wbuttonupdatespeed
                spinnerStart wspinnersort
                on weventboxsort buttonPressEvent $ do
                    button <- eventButton
                    when (button == LeftButton || button == MiddleButton) $
                        liftIO $ killThread tid
                    return True
            forkIO
                ((let
                  callback (Left (board, s)) = postGUIAsync $
                    runE e $ tempError 3 $
                        "Не удалось получить скорость для борды: "
                     ++ renderBoard board ++ ", причина: " ++ s
                  callback (Right (board, s)) = postGUIAsync $ do
                    atomicModifyIORef' currentSort $
                        \m -> (M.insert board s m, ())
                    resortBoards
                in void $
                    sortSsachBoardsByPopularity
                        (postGUIAsync . runE e . writeLog)
                        (Just callback)
                        allSsachBoards
                 ) `finally` (postGUIAsync $ do
                    widgetShow wbuttonupdatespeed
                    widgetHide wspinnersort
                    spinnerStop wspinnersort
                    signalDisconnect connId
                    )
                )
          return ()


        return (boardUnits, wchecksort, currentSort))
    (\(v,wcs,srt) c -> do
        cab <- map buBoard <$>
            filterM (toggleButtonGetActive . buWidget) v
        csbs <- get wcs
        cspd <- get srt
        return c
            { coActiveBoards=cab
            , coSortingByAlphabet=csbs
            , coBoardSpeedData=M.toList cspd
            })
    (\(v,_,_) e -> e{boardUnits=v})

wipebuttonEnvPart :: Builder -> EnvPart
wipebuttonEnvPart b = EP
    (\env _ -> do
        wbuttonwipe <- builderGetObject b castToButton "wipebutton"

        void $ on wbuttonwipe buttonActivated $ do
            ifM (not <$> readTVarIO (wipeStarted env))
                (runE env startWipe)
                (runE env killWipe)

        firstPostStats <- newIORef (Left 0)

        return (wbuttonwipe, firstPostStats))
    (const return)
    (\(wbw, fps) e -> e{wbuttonwipe=wbw, firstPostStats=fps})
