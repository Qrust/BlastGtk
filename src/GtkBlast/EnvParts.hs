{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module GtkBlast.EnvParts
    (envParts
    ,createWidgetsAndFillEnv
    ) where
import Import hiding (on, mod)
import GtkBlast.MuVar
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Pasta
import GtkBlast.Image
import GtkBlast.GuiCaptcha (guiCaptchaEnvPart)
import GtkBlast.AntigateCaptcha (antigateCaptchaEnvPart)
import GtkBlast.Captcha (captchaModeEnvPart)
import GtkBlast.Proxy
import GtkBlast.Conf
import GtkBlast.GtkUtils
import GtkBlast.Mainloop (wipebuttonEnvPart, boardUnitsEnvPart)
import GtkBlast.EnvPart
import BlastItWithPiss
import Graphics.UI.Gtk hiding (get, set, after)
import GHC.Conc
import Control.Concurrent.STM
import Paths_blast_it_with_piss
import Data.Version (showVersion)
import qualified Data.Map as M
import Control.Monad.Fix

import System.FilePath
import System.Directory (getCurrentDirectory)

import Foreign.Ptr
import Foreign.C.String
import System.Glib.GError
import System.Glib.UTFString

import Network.HTTP.Conduit (newManager, managerConnCount)

envParts :: Builder -> [EnvPart]
envParts b =
    [
     guiCaptchaEnvPart b
    ,antigateCaptchaEnvPart b
    ,captchaModeEnvPart b
    ,wipebuttonEnvPart b
    ,boardUnitsEnvPart b
    ,pastaEnvPart b
    ,imageEnvPart b
    ,EP
        (rec coSettingsShown $ builderGetObject b castToExpander "expandersettings")
        (\v c -> get v ? \a -> c{coSettingsShown=a})
        (const id)
    ,EP
        (rec coAdditionalShown $ builderGetObject b castToExpander "expanderadditional")
        (\v c -> get v ? \a -> c{coAdditionalShown=a})
        (const id)
    ,EP
        (\_ _ -> do
            wlabelmessage <- builderGetObject b castToLabel "labelmessage"
            wprogressalignment <- builderGetObject b castToAlignment "progressalignment"
            wprogresswipe <- builderGetObject b castToProgressBar "wipeprogress"

            return (wlabelmessage, wprogressalignment, wprogresswipe))
        (const return)
        (\(wlm, wpa, wpw) e -> e{wlabelmessage=wlm
                                ,wprogressalignment=wpa
                                ,wprogresswipe=wpw
                                })
    ,EP
        (\e c -> do
            walignmentlog <- builderGetObject b castToAlignment "alignmentlog"

            wexpanderlog <- (rec coLogShown $ builderGetObject b castToExpander "expanderlog") e c

            wlabeldetachlog <- builderGetObject b castToLabel "labeldetachlog"
            wlabelattachlog <- builderGetObject b castToLabel "labelattachlog"

            windowlog <- builderGetObject b castToWindow "windowlog"

            wlog <- builderGetObject b castToTextView "log"
            wbuf <- textViewGetBuffer wlog
            wad <- textViewGetVadjustment wlog

            previousPageSize <- newIORef =<< adjustmentGetPageSize wad
            previousUpper <- newIORef =<< adjustmentGetUpper wad

            onAdjChanged wad $ do
                ps <- get previousPageSize
                pu <- get previousUpper
                v <- adjustmentGetValue wad
                size <- adjustmentGetPageSize wad
                upper <- adjustmentGetUpper wad
                when (v >= (pu - ps) || ps <= 0 || size <= 0 || v >= (upper - ps) || v >= (upper - size)) $ do
                    set previousPageSize size
                    set previousUpper upper
                    adjustmentSetValue wad $ upper - size

            let attachedmark = "<a href=\"#\">Открепить лог</a>"

            let detachedmark = "<a href=\"#\">Закрепить лог</a>"

            let detachLog = do
                    containerRemove walignmentlog wexpanderlog
                    containerRemove windowlog wlabelattachlog
                    containerAdd windowlog wexpanderlog
                    containerAdd walignmentlog wlabelattachlog
                    labelSetMarkup wlabeldetachlog detachedmark
                    widgetShow windowlog

            let attachLog = do
                    containerRemove windowlog wexpanderlog
                    containerRemove walignmentlog wlabelattachlog
                    containerAdd walignmentlog wexpanderlog
                    containerAdd windowlog wlabelattachlog
                    labelSetMarkup wlabeldetachlog attachedmark
                    widgetHide windowlog

            detached <- newIORef False

            labelSetMarkup wlabeldetachlog attachedmark

            -- Fucking faggots, why won't they just export these low-level functions?
            let connect_STRING__BOOL
                  :: GObjectClass obj
                  => SignalName
                  -> ConnectAfter
                  -> obj
                  -> (String -> IO Bool)
                  -> IO (ConnectId obj)
                connect_STRING__BOOL signal after obj user =
                    connectGeneric signal after obj action
                  where
                    action :: Ptr GObject -> CString -> IO Bool
                    action _ str1 =
                        failOnGError $
                        peekUTFString str1 >>= \str1' ->
                        user str1'

            on wlabeldetachlog (Signal $ connect_STRING__BOOL "activate-link") $ \_ -> True <$ do
                modM detached $ \detached' -> do
                    if detached'
                      then attachLog
                      else detachLog
                    return (not detached')

            on wlabelattachlog (Signal $ connect_STRING__BOOL "activate-link") $ \_ -> True <$ do
                attachLog
                set detached False

            onDelete windowlog $ \_ -> True <$ do
                attachLog
                set detached False

            return (wbuf,wexpanderlog))
        (\(_,wel) c -> get wel ? \a -> c{coLogShown=a})
        (\(wbuf,_) e -> e{wbuf=wbuf})
    ,EP
        (\e c -> do
            wcheckthread <- (rec coCreateThreads $ builderGetObject b castToCheckButton "check-thread") e c
            wcheckimages <- (rec coAttachImages $ builderGetObject b castToCheckButton "check-images") e c
            wcheckwatermark <- (rec coWatermark $ builderGetObject b castToCheckButton "check-watermark") e c
            wcheckposttimeout <- (rec coUsePostTimeout $ builderGetObject b castToCheckButton "checkposttimeout") e c
            wspinposttimeout <- (rec coPostTimeout $ builderGetObject b castToSpinButton "spinposttimeout") e c
            wcheckthreadtimeout <- (rec coUseThreadTimeout $ builderGetObject b castToCheckButton "checkthreadtimeout") e c
            wspinthreadtimeout <- (rec coThreadTimeout $ builderGetObject b castToSpinButton "spinthreadtimeout") e c
            wcheckfluctuation <- (rec coUseFluctuation $ builderGetObject b castToCheckButton "checkfluctuation") e c
            wspinfluctuation <- (rec coFluctuation $ builderGetObject b castToSpinButton "spinfluctuation") e c
            wchecksage <- (rec coSage $ builderGetObject b castToCheckButton "checksage") e c

            tqOut <- atomically $ newTQueue

            tappendjunkimages <- atomically $ newTVar True
            tpastagen <- atomically $ newTVar emptyPastaGen
            timagegen <- atomically $ newTVar emptyImageGen
            tuseimages <- tvarCheck get wcheckimages
            tcreatethreads <- tvarCheck get wcheckthread
            tmakewatermark <- tvarCheck get wcheckwatermark
            tposttimeout <- tvarSpinCheck get wcheckposttimeout wspinposttimeout
            tthreadtimeout <- tvarSpinCheck get wcheckthreadtimeout wspinthreadtimeout
            tfluctuation <- tvarSpinCheck get wcheckfluctuation wspinfluctuation
            let wchecksageToSageMode True  = SageAccordingToMode
                wchecksageToSageMode False = SageDisabled
            tsagemode <- tvarCheck (fmap wchecksageToSageMode . get) wchecksage

            return (tqOut, ShSettings{..}, wcheckthread, wcheckimages, wcheckwatermark, wcheckposttimeout, wspinposttimeout, wcheckthreadtimeout, wspinthreadtimeout, wcheckfluctuation, wspinfluctuation, wchecksage))
        (\(_,_,wct,wci,wcw,wcpt,wspt,wctt,wstt,wcf,wsf,wcs) c -> do
            ct <- get wct
            ci <- get wci
            cw <- get wcw
            cpt <- get wcpt
            spt <- get wspt
            ctt <- get wctt
            stt <- get wstt
            cf <- get wcf
            sf <- get wsf
            cs <- get wcs
            return c{coCreateThreads=ct
                    ,coAttachImages=ci
                    ,coWatermark=cw
                    ,coUsePostTimeout=cpt
                    ,coPostTimeout=spt
                    ,coUseThreadTimeout=ctt
                    ,coThreadTimeout=stt
                    ,coUseFluctuation=cf
                    ,coFluctuation=sf
                    ,coSage=cs})
        (\(tqOut,shS,_,_,_,_,_,_,_,_,_,_) e ->
            e{tqOut=tqOut
             ,shS=shS
             })
    ,EP
        (rec coAnnoy $ builderGetObject b castToCheckButton "check-annoy")
        (\v c -> get v ? \a -> c{coAnnoy=a})
        (\v e -> e{wcheckannoy=v})
    ,EP
        (rec coHideOnSubmit $ builderGetObject b castToCheckButton "checkhideonsubmit")
        (\v c -> get v ? \a -> c{coHideOnSubmit=a})
        (\v e -> e{wcheckhideonsubmit=v})
    ,EP
        (rec coAnnoyErrors $ builderGetObject b castToCheckButton "check-annoyerrors")
        (\v c -> get v ? \a -> c{coAnnoyErrors=a})
        (\v e -> e{wcheckannoyerrors=v})
    ,EP
        (rec coTray $ builderGetObject b castToCheckButton "check-tray")
        (\v c -> get v ? \a -> c{coTray=a})
        (\v e -> e{wchecktray=v})
    ,EP
        (\e c -> do
            wcheckhttpproxy <- (rec coUseHttpProxy $ builderGetObject b castToCheckButton "checkhttpproxy") e c
            wentryhttpproxyfile <- (rec coHttpProxyFile $ builderGetObject b castToEntry "entryhttpproxyfile") e c
            wbuttonhttpproxyfile <- builderGetObject b castToButton "buttonhttpproxyfile"

            on wcheckhttpproxy buttonActivated $
                runE e $ do
                    set (httpproxyMod e) nullTime -- force update
                    regenerateProxies

            onFileChooserEntryButton False wbuttonhttpproxyfile wentryhttpproxyfile (runE e . writeLog) $
                runE e $ do
                    set (httpproxyMod e) nullTime -- force update
                    regenerateProxies

            return (wcheckhttpproxy, wentryhttpproxyfile))
        (\(wchp, wehpf) c -> do
            chp <- get wchp
            ehpf <- get wehpf
            return c{coUseHttpProxy=chp
                    ,coHttpProxyFile=ehpf})
        (\(wchp, wehpf) e -> e{wcheckhttpproxy=wchp
                              ,wentryhttpproxyfile=wehpf
                              })
    ,EP
        (\e c -> do
            wchecksocksproxy <- (rec coUseSocksProxy $ builderGetObject b castToCheckButton "checksocksproxy") e c
            wentrysocksproxyfile <- (rec coSocksProxyFile $ builderGetObject b castToEntry "entrysocksproxyfile") e c
            wbuttonsocksproxyfile <- builderGetObject b castToButton "buttonsocksproxyfile"

            on wchecksocksproxy buttonActivated $
                runE e $ do
                    set (socksproxyMod e) nullTime -- force update
                    regenerateProxies

            onFileChooserEntryButton False wbuttonsocksproxyfile wentrysocksproxyfile (runE e . writeLog) $
                runE e $ do
                    set (socksproxyMod e) nullTime -- force update
                    regenerateProxies

            return (wchecksocksproxy, wentrysocksproxyfile))
        (\(wcsp, wespf) c -> do
            csp <- get wcsp
            espf <- get wespf
            return c{coUseSocksProxy=csp
                    ,coSocksProxyFile=espf})
        (\(wcsp, wespf) e -> e{wchecksocksproxy=wcsp
                              ,wentrysocksproxyfile=wespf
                              })
    ,EP
        (\ e c -> do
            wchecknoproxy <- (rec coUseNoProxy $ builderGetObject b castToCheckButton "checknoproxy") e c

            on wchecknoproxy buttonActivated $ do
                runE e $ regenerateProxies -- force update

            return wchecknoproxy)
        (\v c -> get v ? \a -> c{coUseNoProxy=a})
        (\v e -> e{wchecknoproxy=v})
    ,EP
        (\_ _ -> do
            wlabelversion <- builderGetObject b castToLabel "labelversion"
            labelSetMarkup wlabelversion $
                "<small><a href=\"https://github.com/exbb2/BlastItWithPiss\">" ++
                    showVersion version ++ "</a></small>")
        (const return)
        (const id)
    ,EP
        (\e _ -> do
            window <- builderGetObject b castToWindow "window1"
            windowSetTitle window "Вайпалка мочана"

            -- setup tray

            wtray <- statusIconNewFromFile $ bundledFile "resources/2ch.so.png"
            statusIconSetTooltip wtray "Вайпалка мочана"
            statusIconSetName wtray "blast-it-with-piss"

            wmenushow <- checkMenuItemNewWithMnemonic "_Показать вайпалку"
            wmenuexit <- imageMenuItemNewFromStock stockQuit
            wmenu <- menuNew
            menuShellAppend wmenu wmenushow
            menuShellAppend wmenu wmenuexit
            widgetShowAll wmenu

            -- tray signals

            on wtray statusIconActivate $ windowToggle window
            on wtray statusIconPopupMenu $ \(Just mb) t -> menuPopup wmenu $ Just (mb, t)
            wmenushowConnId <- on wmenushow menuItemActivate $ windowToggle window
            on wmenuexit menuItemActivate $ widgetDestroy window

            -- window signals

            onDelete window $ \_ -> do
                noTray <- not <$> statusIconIsEmbedded wtray
                closePlease <- not <$> toggleButtonGetActive (wchecktray e)
                if noTray || closePlease
                    then return False
                    else True <$ widgetHide window

            let setCheckActive ca = do
                    signalBlock wmenushowConnId -- prevent it from infinitely showing-unshowing window. I'm unsure if there's a better solution.
                    checkMenuItemSetActive wmenushow ca
                    signalUnblock wmenushowConnId

            onShow window $ setCheckActive True
            onHide window $ setCheckActive False

            widgetShowAll window
            return window
        )
        (const return)
        (\v e -> e{window=v})
    ,EP
        (rec (fromIntegral . coMaxLines) $ builderGetObject b castToSpinButton "wspinmaxlines")
        (\v c -> get v ? \a -> c{coMaxLines=round a})
        (\v e -> e{wspinmaxlines=v})
    ,EP (\ _ _ -> do
            wlabellogfile <- builderGetObject b castToLabel "labellogfile"

            _pwd <- getCurrentDirectory
            let
              file =
#ifdef mingw32_HOST_OS
                '/' : _pwd </> "log.txt"
#else
                _pwd </> "log.txt"
#endif
            labelSetMarkup wlabellogfile $
                "<a href=\"file://" ++ file ++ "\">log.txt</a>"
        )
        (const return)
        (const id)
    ]

createWidgetsAndFillEnv :: Builder -> Conf -> IO (Env, Conf -> IO Conf)
createWidgetsAndFillEnv builder conf = do
    messageLocks <- newIORef 0

    wipeStarted <- newIORef False

    postCount <- newIORef 0
    wipeStats <- newIORef (0, 0, 0)

    proxies <- newIORef M.empty

    httpproxyMod <- newIORef nullTime
    httpproxyLast <- newIORef []

    socksproxyMod <- newIORef nullTime
    socksproxyLast <- newIORef []

    connection <- newManager def{managerConnCount=20000}

    (re, rs) <- mfix $ \ ~(lolhaskell, _) -> do
        (setEnv, setConf) <- runEnvParts (envParts builder) lolhaskell conf
        return (setEnv E{..}, setConf)
    return (force re, rs)
