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
import GtkBlast.GuiCaptcha (guiCaptchaEnvPart)
import GtkBlast.AntigateCaptcha (antigateCaptchaEnvPart)
import GtkBlast.Captcha (captchaModeEnvPart)
import GtkBlast.Proxy
import GtkBlast.Conf
import GtkBlast.GtkUtils
import GtkBlast.Mainloop (wipebuttonEnvPart, boardUnitsEnvPart)
import GtkBlast.EnvPart
import "blast-it-with-piss" BlastItWithPiss
import Graphics.UI.Gtk hiding (get, set)
import GHC.Conc
import Control.Concurrent.STM
import Paths_blast_it_with_piss
import Data.Version (showVersion)
import qualified Data.Map as M
import Control.Monad.Fix

envParts :: Builder -> [EnvPart]
envParts b =
    let build :: GObjectClass cls => (GObject -> cls) -> String -> IO cls
        build f n = builderGetObject b f n
    in
    [
     guiCaptchaEnvPart b
    ,antigateCaptchaEnvPart b
    ,captchaModeEnvPart b
    ,wipebuttonEnvPart b
    ,boardUnitsEnvPart b
    ,pastaEnvPart b
    ,EP
        (rec coSettingsShown $ build castToExpander "expandersettings")
        (\v c -> get v ? \a -> c{coSettingsShown=a})
        (const id)
    ,EP
        (rec coAdditionalShown $ build castToExpander "expanderadditional")
        (\v c -> get v ? \a -> c{coAdditionalShown=a})
        (const id)
    ,EP
        (rec coLogShown $ build castToExpander "expanderlog")
        (\v c -> get v ? \a -> c{coLogShown=a})
        (const id)
    ,EP
        (\_ _ -> do
            wlabelmessage <- build castToLabel "labelmessage"
            wprogressalignment <- build castToAlignment "progressalignment"
            wprogresswipe <- build castToProgressBar "wipeprogress"

            wlog <- build castToTextView "log"
            wbuf <- textViewGetBuffer wlog
            wad <- textViewGetVadjustment wlog

            previousPageSize <- newIORef =<< adjustmentGetPageSize wad
            previousUpper <- newIORef =<< adjustmentGetUpper wad

            onAdjChanged wad $ do
                v <- adjustmentGetValue wad
                ps <- readIORef previousPageSize
                pu <- subtract ps <$> readIORef previousUpper
                when (v >= pu) $ do
                    s <- adjustmentGetPageSize wad
                    u <- adjustmentGetUpper wad
                    adjustmentSetValue wad $ subtract s u
                    writeIORef previousPageSize s
                    writeIORef previousUpper u
            
            return (wlabelmessage, wprogressalignment, wprogresswipe, wbuf))
        (const return)
        (\(wlm, wpa, wpw, wbuf) e -> e {wlabelmessage=wlm
                                       ,wprogressalignment=wpa
                                       ,wprogresswipe=wpw
                                       ,wbuf = wbuf
                                       })
    ,EP
        (\e c -> do
            wcheckthread <- (rec coCreateThreads $ build castToCheckButton "check-thread") e c
            wcheckimages <- (rec coAttachImages $ build castToCheckButton "check-images") e c
            wcheckwatermark <- (rec coWatermark $ build castToCheckButton "check-watermark") e c

            tqOut <- atomically $ newTQueue

            tpastagen <- atomically $ newTVar $ \_ _ _ -> return ((True, True), "Генератор не запущен. Осторожно, двери закрываются.")
            timages <- atomically $ newTVar []
            tuseimages <- atomically . newTVar =<< toggleButtonGetActive wcheckimages
            tcreatethreads <- atomically . newTVar =<< toggleButtonGetActive wcheckthread
            tmakewatermark <- atomically . newTVar =<< toggleButtonGetActive wcheckwatermark
            tappendjunkimages <- atomically $ newTVar True

            on wcheckimages toggled $
                atomically . writeTVar tuseimages =<< toggleButtonGetActive wcheckimages
        
            on wcheckthread toggled $
                atomically . writeTVar tcreatethreads =<< toggleButtonGetActive wcheckthread

            on wcheckwatermark toggled $
                atomically . writeTVar tmakewatermark =<< toggleButtonGetActive wcheckwatermark

            return (tqOut, ShSettings{..}, wcheckthread, wcheckimages, wcheckwatermark))
        (\(_, _, wct, wci, wcw) c -> do
            ct <- get wct
            ci <- get wci
            cw <- get wcw
            return c{coCreateThreads=ct
                    ,coAttachImages=ci
                    ,coWatermark=cw}
            )
        (\(tqOut, shS, _, wcheckimages, _) e -> e {tqOut=tqOut
                                                  ,shS=shS
                                                  ,wcheckimages=wcheckimages
                                                  })
    ,EP
        (rec coAnnoy $ build castToCheckButton "check-annoy")
        (\v c -> get v ? \a -> c{coAnnoy=a})
        (\v e -> e{wcheckannoy=v})
    ,EP
        (rec coHideOnSubmit $ build castToCheckButton "checkhideonsubmit")
        (\v c -> get v ? \a -> c{coHideOnSubmit=a})
        (\v e -> e{wcheckhideonsubmit=v})
    ,EP
        (rec coAnnoyErrors $ build castToCheckButton "check-annoyerrors")
        (\v c -> get v ? \a -> c{coAnnoyErrors=a})
        (\v e -> e{wcheckannoyerrors=v})
    ,EP
        (rec coTray $ build castToCheckButton "check-tray")
        (\v c -> get v ? \a -> c{coTray=a})
        (\v e -> e{wchecktray=v})
    ,EP
        (\e c -> do
            wentryimagefolder <- (rec coImageFolder $ build castToEntry "entryimagefolder") e c
            wbuttonimagefolder <- build castToButton "buttonimagefolder"
            
            onFileChooserEntryButton True wbuttonimagefolder wentryimagefolder (runE e . writeLog) (return ())

            return wentryimagefolder)
        (\v c -> get v ? \a -> c{coImageFolder=a})
        (\v e -> e{wentryimagefolder=v})
    ,EP
        (\env _ -> do
            wbuttonselectall <- build castToButton "buttonselectall"
            wbuttonselectnone <- build castToButton "buttonselectnone"

            on wbuttonselectall buttonActivated $ do
                forM_ (boardUnits env) $
                    (`toggleButtonSetActive` True) . buWidget
        
            on wbuttonselectnone buttonActivated $ do
                forM_ (boardUnits env) $
                    (`toggleButtonSetActive` False) . buWidget)
        (const return)
        (const id)
    ,EP
        (\e c -> do
            wcheckhttpproxy <- (rec coUseHttpProxy $ build castToCheckButton "checkhttpproxy") e c
            wentryhttpproxyfile <- (rec coHttpProxyFile $ build castToEntry "entryhttpproxyfile") e c
            wbuttonhttpproxyfile <- build castToButton "buttonhttpproxyfile"

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
            wchecksocksproxy <- (rec coUseSocksProxy $ build castToCheckButton "checksocksproxy") e c
            wentrysocksproxyfile <- (rec coSocksProxyFile $ build castToEntry "entrysocksproxyfile") e c
            wbuttonsocksproxyfile <- build castToButton "buttonsocksproxyfile"

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
            wchecknoproxy <- (rec coUseNoProxy $ build castToCheckButton "checknoproxy") e c

            on wchecknoproxy buttonActivated $ do
                runE e $ regenerateProxies -- force update

            return wchecknoproxy)
        (\v c -> get v ? \a -> c{coUseNoProxy=a})
        (\v e -> e{wchecknoproxy=v})
    ,EP
        (\_ _ -> do
            wlabelversion <- build castToLabel "labelversion"
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
        
            wtray <- statusIconNewFromFile =<< getResourceFile "2ch.so.png"
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
    ]

createWidgetsAndFillEnv :: Builder -> Conf -> IO (Env, Conf -> IO Conf)
createWidgetsAndFillEnv builder conf = do
    messageLocks <- newIORef 0
  
    wipeStarted <- newIORef False
  
    postCount <- newIORef 0
    wipeStats <- newIORef (0, 0, 0)

    imagesLast <- newIORef []

    proxies <- newIORef M.empty

    httpproxyMod <- newIORef nullTime
    httpproxyLast <- newIORef []

    socksproxyMod <- newIORef nullTime
    socksproxyLast <- newIORef []

    (re, rs) <- mfix $ \ ~(lolhaskell, _) -> do
        (setEnv, setConf) <- runEnvParts (envParts builder) lolhaskell conf
        return (setEnv E{..}, setConf)
    return (force re, rs)
