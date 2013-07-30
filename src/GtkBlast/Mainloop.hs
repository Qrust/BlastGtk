{-# OPTIONS_GHC -Wall #-}
module GtkBlast.Mainloop
    (wipebuttonEnvPart
    ,boardUnitsEnvPart
    ,mainloop
    ,setMainLoop
    ) where
import Import hiding (on, mod)
import qualified Data.Function as F (on)

import Paths_blast_it_with_piss

import GtkBlast.Worker
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
import BlastItWithPiss.Blast
import BlastItWithPiss.Choice (Mode(..))
import BlastItWithPiss.Parsing
import BlastItWithPiss.Board

import qualified Data.Text as T
import qualified Data.Map as M

import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as G (set)

import GHC.Conc hiding (forkIO)
import qualified Control.Concurrent.Thread.Group as ThreadGroup

updWipeMessage :: E ()
updWipeMessage = do
    E{..} <- ask
    whenM (get wipeStarted) $ do
        pc <- get postCount
        (active, banned, dead) <- get wipeStats
        let ach = getAchievementString pc
        -- HACK clear message
        updMessage ""
        io $ labelSetText wlabelstats $ T.unpack $
               "Постов: " ++ show pc ++ " / "
            ++ "Активно: " ++ show active
            ++ " / Забанено: " ++ show banned
            ++ (if dead > 0 then " / Наебнулось: " ++ show dead else "")
            ++ (if T.null ach then "" else "\n" ++ ach)

killWipeUnit :: Board -> WipeUnit -> E ()
killWipeUnit board WipeUnit{..} = do
    writeLog $ "Killing thread for " ++ renderBoard board ++ " {" ++ show wuProxy ++ "}"
    io $ killThread wuThreadId
    writeLog $ "Killed thread for " ++ renderBoard board ++ " {" ++ show wuProxy ++ "}"

killBoardUnitWipeUnits :: BoardUnit -> E ()
killBoardUnitWipeUnits BoardUnit{..} = do
    mapM_ (killWipeUnit buBoard) =<< get buWipeUnits
    set buWipeUnits []

cleanBoardUnitBadRecord :: BoardUnit -> E ()
cleanBoardUnitBadRecord BoardUnit{..} = do
    unlessM (null <$> get buBanned) $ do
        writeLog $ "Cleaning board unit " ++ renderBoard buBoard ++ " banned proxy records"
        set buBanned []
    unlessM (null <$> get buDead) $ do
        writeLog $ "Cleaning board unit " ++ renderBoard buBoard ++ " dead proxy records"
        set buDead []

killBoardUnit :: BoardUnit -> E ()
killBoardUnit bu = do
    killBoardUnitWipeUnits bu
    cleanBoardUnitBadRecord bu

asyncReactToMessage :: Env -> (OutMessage -> IO ())
asyncReactToMessage env = postGUIAsync . runE env . reactToMessage

newWipeUnit
    :: Board
    -> BlastProxy
    -> MuSettings
    -> ProxySettings
    -> E WipeUnit
newWipeUnit board bproxy muSettings proxySettings = do
    env@E{..} <- ask
    writeLog $
        "Spawning new thread for " ++
        renderBoard board ++ " {" ++
        show bproxy ++ "}"
#ifdef mingw32_HOST_OS
    threadid <- fmap fst $ io $ ThreadGroup.forkOS threadGroup $ do
#else
    threadid <- fmap fst $ io $ ThreadGroup.forkIO threadGroup $ do
#endif
        entryPoint connection bproxy board Log shS muSettings proxySettings $
            asyncReactToMessage env
    writeLog $ "Spawned " ++ renderBoard board ++ " {" ++ show bproxy ++ "}"
    return $ WipeUnit bproxy threadid

regenerateExcluding :: Board -> [BlastProxy] -> MuSettings -> E [WipeUnit]
regenerateExcluding board excludeThese muSettings = do
    E{..} <- ask
    prx <- M.assocs <$> get proxies
    forMaybeM prx $ \(bproxy, prSettings) ->
        if bproxy `elem` excludeThese
          then
            return Nothing
          else do
            Just <$> newWipeUnit board bproxy muSettings prSettings

maintainWipeUnit
    :: BoardUnit
    -> Bool
    -> Bool
    -> WipeUnit
    -> E (Maybe (Either BlastProxy WipeUnit))
maintainWipeUnit BoardUnit{..} isActive hasWipeStarted w@WipeUnit{..} = do
    E{..} <- ask
    st <- io $ threadStatus wuThreadId
    pxs <- get proxies
    if st == ThreadDied || st == ThreadFinished
      then do
        writeLog $
            "blasgtk: Thread for {" ++ show wuProxy
            ++ "} " ++ renderBoard buBoard ++
            " died. Removing"
        return $ Just $ Left wuProxy
      else
        if not isActive || not hasWipeStarted || M.notMember wuProxy pxs
          then do
            writeLog $
                "Removing unneded {" ++ show wuProxy
                ++ "} " ++ renderBoard buBoard
            killWipeUnit buBoard w >> return Nothing
          else
            return $ Just $ Right w

maintainBoardUnit
    :: (Int, [(Board, [BlastProxy])], [(Board, [BlastProxy])])
    -> BoardUnit
    -> E (Int, [(Board, [BlastProxy])], [(Board, [BlastProxy])])
maintainBoardUnit (!activecount, !pbanned, !pdead) bu@BoardUnit{..} = do
    E{..} <- ask
    isActive <- get buWidget
    hasWipeStarted <- get wipeStarted
    _wipeUnits <- get buWipeUnits
    (newdead, old) <-
        fmap partitionEithers $ forMaybeM _wipeUnits $ \wu ->
            maintainWipeUnit bu isActive hasWipeStarted wu
    mod buDead (newdead++)
    banned <- get buBanned
    dead <- get buDead
    new <-
        if isActive && hasWipeStarted
          then do
            regenerateExcluding buBoard (map wuProxy old ++ banned ++ dead) buMuSettings
          else
            return []
    let newwus = old ++ new
    set buWipeUnits newwus
    if isActive
      then return $
        (activecount + length newwus
        ,(buBoard, banned) : pbanned
        ,(buBoard, dead) : pdead
        )
      else return $
        (activecount, pbanned, pdead)

maintainBoardUnits :: E [(Board, [BlastProxy])]
maintainBoardUnits = do
    E{..} <- ask
    (active, banned, dead) <- foldM maintainBoardUnit (0,[],[]) boardUnits
    let !bannedCount = length $ concatMap snd banned
        !deadCount = length $ concatMap snd dead
    set wipeStats (active, bannedCount, deadCount)
    when (active == 0) $ do
        killWipe
        ifM (M.null <$> get proxies)
            (redMessage "Нет проксей, выберите прокси для вайпа или вайпайте без прокси")
            (if bannedCount > 0 || deadCount > 0
              then
                return () -- uncAnnoyMessage "Все треды забанены или наебнулись. Выберите другие доски для вайпа или найдите прокси не являющиеся калом ёбаным."
              else
                redMessage "Выберите доски для вайпа")
    return (banned ++ dead)

startWipe :: E ()
startWipe = do
    E{..} <- ask
    writeLog "Starting wipe..."
    uncMessage "Заряжаем пушки..."
    set wipeStarted True
    io $ buttonSetLabel wbuttonwipe "Прекратить _Вайп"
    io $ progressBarPulse wprogresswipe

killWipe :: E ()
killWipe = do
    E{..} <- ask

    writeLog "Stopping wipe..."

    set wipeStarted False

    -- close all connections
    io $ closeManager connection -- TODO FIXME CLARIFY

    mapM_ killBoardUnit boardUnits
    io $ ThreadGroup.wait threadGroup
    -- remove captcha, once all generators are dead
    killAllCaptcha

    io $ buttonSetLabel wbuttonwipe "Начать _Вайп"
    io $ progressBarSetFraction wprogresswipe 0
    --uncMessage "Вайп ещё не начат"

setBanned :: Board -> BlastProxy -> E ()
setBanned board proxy = do
    writeLog $ "setBanned: " ++ renderBoard board ++ " {" ++ show proxy ++ "}"
    bus <- asks boardUnits
    whenJust (find ((==board) . buBoard) bus) $ \BoardUnit{..} -> do
        bwus <- get buWipeUnits
        whenJust (find ((==proxy) . wuProxy) bwus) $ \wu -> do
            writeLog $ "Banning " ++ renderBoard board ++ " {" ++ show proxy ++ "}"
            killWipeUnit buBoard wu
            mod buWipeUnits $ delete wu
            mod buBanned (wuProxy wu :)

addPost :: E ()
addPost = do
    E{..} <- ask
    modi (+1) postCount
    writeLog =<< ("Updated post count: " ++) . show <$> get postCount

reactToMessage :: OutMessage -> E ()
reactToMessage s@(OutMessage st@(OriginStamp _ proxy board _ _) m) = do
    E{..} <- ask
    case m of
        LogMessage _ -> writeLog (show s)
        OutcomeMessage o -> do
            case o of
                SuccessLongPost _ -> writeLog (show st ++ ": SuccessLongPost")
                _ -> writeLog (show s)
            case o of
                Success -> addPost
                SuccessLongPost _ -> addPost
                Wordfilter -> tempError 3 "Не удалось обойти вордфильтр"
                ThreadDoesNotExist -> tempError 5 "Тред удалён"
                SameMessage -> tempError 2 $ stamp $ "Вы уже постили это сообщение"
                SameImage -> tempError 2 $ stamp $ "Этот файл уже загружен"
                TooFastPost -> writeLog $ stamp $ "Вы постите слишком часто, умерьте пыл"
                TooFastThread -> tempError 3 $ stamp $ "Вы создаете треды слишком часто"
                NeedCaptcha -> writeLog $ stamp $ "NeedCaptcha"
                WrongCaptcha -> writeLog $ stamp $ "WrongCaptcha"
                LongPost -> tempError 1 $ stamp $ "Запостил слишком длинный пост"
                EmptyPost -> tempError 2 $ stamp $ "Вы ничего не написали в сообщении и не прикрепили картинку"
                CorruptedImage -> tempError 2 $ stamp $ "Запостил поврежденное изображение"
                PostRejected -> writeLog $ stamp $ "PostRejected"
                OtherError x -> tempError 4 $ stamp $ "" ++ show x
                InternalError x -> tempError 4 $ stamp $ "" ++ show x
                Banned x -> do
                    banMessage 5 $ "Забанен " ++ renderCompactStamp st
                                ++ " Причина: " ++ show x
                                ++ "\nВозможно стоит переподключится или начать вайпать /d/"
                    setBanned board proxy
                RecaptchaBan -> do
                    banMessage 7 $ stamp $ "Забанен рекапчой, охуеть."
                    setBanned board proxy
                CloudflareCaptcha -> do
                    banMessage 2 $ stamp $ "Если эта ошибка появляется то это баг, сообщите нам об этом (Невозможно обойоти капчу клаудфлера во время постинга https://github.com/exbb2/BlastItWithPiss/issues/4)"
                    setBanned board proxy
                CloudflareBan -> do
                    banMessage 2 $ stamp $ "Бан по клаудфлеру"
                    setBanned board proxy
                Four'o'FourBan -> do
                    banMessage 2 $ stamp $ "Бан по 404"
                    setBanned board proxy
                Four'o'ThreeBan -> do
                    banMessage 2 $ stamp $ "Бан по 403"
                    setBanned board proxy
                Five'o'ThreeError -> writeLog $ stamp $ "Five'o'ThreeError"
                UnknownError -> tempError 4 $ stamp $ "Неизвестная ошибка, что-то пошло не так"
        SolveCaptcha c -> addCaptcha (st, c)
        NoPastas -> do writeLog (show s)
                       tempError 3 "Невозможно прочитать пасты, постим нихуя"
        NoImages -> do writeLog (show s)
                       tempError 3 "Невозможно прочитать пикчи, постим капчу"
  where stamp msg = renderCompactStamp st ++ ": " ++ msg

mainloop :: E ()
mainloop = do
    E{..} <- ask
    whenM (get wipeStarted) $ do
        regenerateProxies
        bus <- maintainBoardUnits
        maintainCaptcha bus
    updWipeMessage

setMainLoop :: Default Conf => Env -> FilePath -> (Conf -> IO Conf) -> IO ()
setMainLoop env configfile setConf = do
    runE env $ writeLog "Setting timeouts."
    void $ timeoutAddFull (do
        whenM (get $ wipeStarted env) $ do
            progressBarPulse $ wprogresswipe env
        return True) priorityLow 30
    void $ timeoutAddFull (do
        runE env mainloop
        return True) priorityDefaultIdle 50 --kiloseconds, 20 fps.
    void $ onDestroy (window env) $ runE env $ do
        writeLog "Closing gtkblast"
        conf <- io $ setConf
            def { coFirstLaunch=False
                , coLastVersion=GtkBlastVersion version
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
        writeLog $ "showBoardSettings: ERROR couldn't find boardunit for " ++ renderBoard board
      Just BoardUnit{buMuSettings=MuSettings{..}} -> io $ do
        b <- builderNew
        builderAddFromString b $ boardSettingsGuiXML board
        window <- builderGetObject b castToWindow "window1"

        mtrd <- readTVarIO mthread
        mmod <- readTVarIO mmode
        mposttm <- readTVarIO mposttimeout
        mtrdtm <- readTVarIO mthreadtimeout

        walignmentthread <- builderGetObject b castToAlignment "alignmentthread"
        wcheckwipethread <- builderGetObject b castToCheckButton "checkwipethread"
        set wcheckwipethread $ isJust mtrd && isJust mmod
        widgetSetSensitive walignmentthread $ isJust mtrd && isJust mmod

        wspinthreadnum <- builderGetObject b castToSpinButton "spinthreadnum"
        whenJust mtrd $ set wspinthreadnum . fromIntegral

        wchecksage <- builderGetObject b castToCheckButton "checksage"
        whenJust mmod $ \mode -> do
            if mode /= SagePopular && mode /= BumpUnpopular
                then do
                runE e $ tempError 3 "TERRIBLE! showBoardSettings ERROR: Unknown mode."
                toggleButtonSetInconsistent wchecksage True
                else do
                set wchecksage (mode==SagePopular)

        wcheckposttimeout <- builderGetObject b castToCheckButton "checkposttimeout"
        set wcheckposttimeout $ isJust mposttm

        wspinposttimeout <- builderGetObject b castToSpinButton "spinposttimeout"
        set wspinposttimeout $ fromMaybe (ssachPostTimeout board) mposttm

        wcheckthreadtimeout <- builderGetObject b castToCheckButton "checkthreadtimeout"
        set wcheckthreadtimeout $ isJust mtrdtm

        wspinthreadtimeout <- builderGetObject b castToSpinButton "spinthreadtimeout"
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
            runE e $ writeLog $ renderBoard board ++ ": new thread: " ++ show nmthread
            atomically $ writeTVar mthread nmthread

            nmmode <- ifMJust (get wcheckwipethread)
                            (ifM (get wchecksage)
                                (return SagePopular)
                                (return BumpUnpopular))
            atomically $ writeTVar mmode nmmode
            runE e $ writeLog $ renderBoard board ++ ": new mode: " ++ show nmmode

            nmposttimeout <- ifMJust (get wcheckposttimeout)
                            (get wspinposttimeout)
            atomically $ writeTVar mposttimeout nmposttimeout
            runE e $ writeLog $ renderBoard board ++ ": new post timeout: " ++ show nmposttimeout

            nmthreadtimeout <- ifMJust (get wcheckthreadtimeout)
                            (get wspinthreadtimeout)
            atomically $ writeTVar mthreadtimeout nmthreadtimeout
            runE e $ writeLog $ renderBoard board ++ ": new thread timeout: " ++ show nmthreadtimeout
            -- TODO Config read/write post timeout & thread timeout

        widgetShowAll window

boardUnitsEnvPart :: Builder -> EnvPart
boardUnitsEnvPart b = EP
    (\e c -> do
        let ssachBoardsWithSpeed =
                if coSortingByAlphabet c
                    then sortBy (compare `F.on` fst) ssachBoardsSortedByPostRate
                    else ssachBoardsSortedByPostRate

        wvboxboards <- builderGetObject b castToVBox "vbox-boards"

        boardUnits <- forM ssachBoardsWithSpeed $ \(board, sp) -> do
            whb <- hBoxNew False 0

            wc <- checkButtonNewWithLabel $ renderBoard board
            when (board `elem` coActiveBoards c) $ toggleButtonSetActive wc True
            G.set wc [widgetTooltipText := Just (show sp ++ " п./час")]

            wb <- buttonNew
            containerAdd wb =<< imageNewFromStock stockEdit IconSizeMenu
            buttonSetRelief wb ReliefNone
            G.set wb [widgetTooltipText := Just ("Настроить вайп " ++ renderBoard board)]
            void $ on wb buttonActivated $ do
                runE e $ showBoardSettings board

            boxPackStart whb wc PackGrow 0
            boxPackStart whb wb PackNatural 0

            boxPackStart wvboxboards whb PackNatural 0
            BoardUnit board wc <$> newIORef [] <*> newIORef [] <*> newIORef [] <*> defMuS

        wbuttonselectall <- builderGetObject b castToButton "buttonselectall"
        wbuttonselectnone <- builderGetObject b castToButton "buttonselectnone"
        wchecksort <- setir (coSortingByAlphabet c) =<< builderGetObject b castToCheckButton "checksort"

        void $ on wbuttonselectall buttonActivated $ do
            forM_ boardUnits $
                (`toggleButtonSetActive` True) . buWidget

        void $ on wbuttonselectnone buttonActivated $ do
            forM_ boardUnits $
                (`toggleButtonSetActive` False) . buWidget

        void $ on wchecksort buttonActivated $ do
            spd <- get wchecksort
            let
              reorderedBoards =
                if spd
                  then
                    sortBy (compare `F.on` buBoard) boardUnits
                  else
                    flip sortBy boardUnits (
                      compare `F.on` \bu ->
                        fromMaybe (fromEnum $ buBoard bu) $
                          findIndex ((== buBoard bu) . fst) ssachBoardsSortedByPostRate)
            foldM_ (\ !i bu -> do
                maybeParent <- widgetGetParent (buWidget bu)
                whenJust maybeParent $ \w ->
                    boxReorderChild wvboxboards w i
                return $ i+1
                ) 0 reorderedBoards

        return (boardUnits, wchecksort))
    (\(v,wcs) c -> do
        cab <- map buBoard <$>
            filterM (toggleButtonGetActive . buWidget) v
        csbs <- get wcs
        return c{coActiveBoards=cab, coSortingByAlphabet=csbs})
    (\(v,_) e -> e{boardUnits=v})

wipebuttonEnvPart :: Builder -> EnvPart
wipebuttonEnvPart b = EP
    (\env _ -> do
        wbuttonwipe <- builderGetObject b castToButton "wipebutton"

        void $ on wbuttonwipe buttonActivated $ do
            ifM (not <$> readIORef (wipeStarted env))
                (runE env startWipe)
                (runE env killWipe)

        return wbuttonwipe)
    (const return)
    (\v e -> e{wbuttonwipe=v})
