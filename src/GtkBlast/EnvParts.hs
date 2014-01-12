{-# OPTIONS_GHC
    -fno-warn-unused-do-bind
    -fno-warn-missing-fields
    #-}
module GtkBlast.EnvParts
    (envParts
    ,createWidgetsAndFillEnv
    ) where
import Import hiding (on, mod)

import Paths_blast_it_with_piss

import GtkBlast.MuVar
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Pasta
import GtkBlast.Image
import GtkBlast.Video
import GtkBlast.GuiCaptcha (guiCaptchaEnvPart)
import GtkBlast.AntigateCaptcha (antigateCaptchaEnvPart)
import GtkBlast.Captcha (captchaModeEnvPart)
import GtkBlast.Proxy
import GtkBlast.Conf
import GtkBlast.GtkUtils
import GtkBlast.Mainloop
    ( wipebuttonEnvPart
    , boardUnitsEnvPart
    , presolveCaptchaEnvPart
    )
import GtkBlast.EnvPart

import BlastItWithPiss
import BlastItWithPiss.Choice (Mode(CreateNew))
import Control.Concurrent.STM.FinalizerTVar

import Graphics.UI.Gtk hiding (get, set, after)

import Foreign.Ptr
import Foreign.C.String
import System.Glib.GError
import System.Glib.UTFString

import GHC.Conc
-- import Control.Concurrent.STM
import qualified Control.Concurrent.Thread.Group as ThreadGroup

import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.Set as Set

import Control.Monad.Fix

import System.FilePath
import System.Directory (getCurrentDirectory)

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
    ,videoEnvPart b
    ,imageEnvPart b

    ,presolveCaptchaEnvPart b

    ,EP
        (\_ _ -> do
            wlabelstats <- builderGetObject b castToLabel "labelstats"
            wlabelmessage <- builderGetObject b castToLabel "labelmessage"
            wprogressalignment <- builderGetObject b castToAlignment "progressalignment"
            wprogresswipe <- builderGetObject b castToProgressBar "wipeprogress"

            return (wlabelstats, wlabelmessage, wprogressalignment, wprogresswipe))
        (const return)
        (\(wls,wlm, wpa, wpw) e ->
            e{wlabelstats=wls
             ,wlabelmessage=wlm
             ,wprogressalignment=wpa
             ,wprogresswipe=wpw
             })
    ,EP
        (\_ _ -> do
            walignmentlog <- builderGetObject b castToAlignment "alignmentlog"

            wvboxlog <- builderGetObject b castToVBox "wvboxlog"

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
                    containerRemove walignmentlog wvboxlog
                    containerRemove windowlog wlabelattachlog
                    containerAdd windowlog wvboxlog
                    containerAdd walignmentlog wlabelattachlog
                    labelSetMarkup wlabeldetachlog detachedmark
                    widgetShow windowlog

            let attachLog = do
                    containerRemove windowlog wvboxlog
                    containerRemove walignmentlog wlabelattachlog
                    containerAdd walignmentlog wvboxlog
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

            return wbuf)
        (const return)
        (\wbuf e -> e{wbuf=wbuf})
    ,EP
        (\e c -> do
            wcheckthread <- setir (coCreateThreads c)
                    =<< builderGetObject b castToCheckButton "check-thread"
            wcheckimages <- setir (coAttachImages c)
                    =<< builderGetObject b castToCheckButton "check-images"
            wcheckwatermark <- setir (coWatermark c)
                    =<< builderGetObject b castToCheckButton "check-watermark"
            wcheckposttimeout <- setir (coUsePostTimeout c)
                    =<< builderGetObject b castToCheckButton "checkposttimeout"
            wspinposttimeout <- setir (coPostTimeout c)
                    =<< builderGetObject b castToSpinButton "spinposttimeout"
            wcheckthreadtimeout <- setir (coUseThreadTimeout c)
                    =<< builderGetObject b castToCheckButton "checkthreadtimeout"
            wspinthreadtimeout <- setir (coThreadTimeout c)
                    =<< builderGetObject b castToSpinButton "spinthreadtimeout"
            wcheckfluctuation <- setir (coUseFluctuation c)
                    =<< builderGetObject b castToCheckButton "checkfluctuation"
            wspinfluctuation <- setir (coFluctuation c)
                    =<< builderGetObject b castToSpinButton "spinfluctuation"
            wchecksage <- setir (coSage c)
                    =<< builderGetObject b castToCheckButton "checksage"

            tappendjunkimages <- newTVarIO True
            tpastagen <- newTVarIO emptyPastaGen
            timagegen <- newTVarIO emptyImageGen
            tvideogen <- newTVarIO emptyVideoGen
            tuseimages <- tvarCheck get wcheckimages

            tallowedmodes <- tvarCheck
                (\w -> do
                    on' <- get w
                    return $ if on'
                      then Set.fromList allModes
                      else Set.delete CreateNew (Set.fromList allModes)
                ) wcheckthread

            tmakewatermark <- tvarCheck get wcheckwatermark
            tposttimeout <- tvarSpinCheck get wcheckposttimeout wspinposttimeout
            tthreadtimeout <- tvarSpinCheck get wcheckthreadtimeout wspinthreadtimeout
            tfluctuation <- tvarSpinCheck get wcheckfluctuation wspinfluctuation
            let wchecksageToSageMode True  = SageAccordingToMode
                wchecksageToSageMode False = SageDisabled
            tsagemode <- tvarCheck (fmap wchecksageToSageMode . get) wchecksage

            -- added in 1.2
            tcaptchaserver <- newFinalizerTVarIO -- HACK dummy captcha server
                (CaptchaServer $ \_ -> do
                    liftIO $ runE e $ writeLog $
                      "AAAAAAAAAAAAH, dummy captcha server invoked, report bug "
                      ++ "https://github.com/exbb2/BlastItWithPiss/issues"
                    return Nothing
                )
                (runE e $ writeLog "dummy captcha server finalized...")
            tstartsignal <- newTVarIO False

            return (ShSettings{..}
                , wcheckthread
                , wcheckimages
                , wcheckwatermark
                , wcheckposttimeout
                , wspinposttimeout
                , wcheckthreadtimeout
                , wspinthreadtimeout
                , wcheckfluctuation
                , wspinfluctuation
                , wchecksage))
        (\(_,wct,wci,wcw,wcpt,wspt,wctt,wstt,wcf,wsf,wcs) c -> do
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
        (\(shS,_,_,_,_,_,_,_,_,_,_) e ->
            e{shS=shS
             })
    ,EP
        (\_ c -> setir (coAnnoy c) =<< builderGetObject b castToCheckButton "check-annoy")
        (\v c -> get v <&> \a -> c{coAnnoy=a})
        (\v e -> e{wcheckannoy=v})
    ,EP
        (\_ c -> setir (coHideOnSubmit c) =<< builderGetObject b castToCheckButton "checkhideonsubmit")
        (\v c -> get v <&> \a -> c{coHideOnSubmit=a})
        (\v e -> e{wcheckhideonsubmit=v})
    ,EP
        (\_ c -> setir (coAnnoyErrors c) =<< builderGetObject b castToCheckButton "check-annoyerrors")
        (\v c -> get v <&> \a -> c{coAnnoyErrors=a})
        (\v e -> e{wcheckannoyerrors=v})
    ,EP
        (\e c -> do
            wcheckhttpproxy <- setir (coUseHttpProxy c) =<< builderGetObject b castToCheckButton "checkhttpproxy"
            wentryhttpproxyfile <- setir (coHttpProxyFile c) =<< builderGetObject b castToEntry "entryhttpproxyfile"
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
            wchecksocksproxy <- setir (coUseSocksProxy c) =<< builderGetObject b castToCheckButton "checksocksproxy"
            wentrysocksproxyfile <- setir (coSocksProxyFile c) =<< builderGetObject b castToEntry "entrysocksproxyfile"
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
            wchecknoproxy <- setir (coUseNoProxy c) =<< builderGetObject b castToCheckButton "checknoproxy"

            on wchecknoproxy buttonActivated $ do
                runE e $ regenerateProxies -- force update

            return wchecknoproxy)
        (\v c -> get v <&> \a -> c{coUseNoProxy=a})
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
        (\_ c -> do
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

            wchecktray <- setir (coTray c)
                =<< builderGetObject b castToCheckButton "check-tray"

            on wtray statusIconActivate $ windowToggle window
            on wtray statusIconPopupMenu $ \(Just mb) t -> menuPopup wmenu $ Just (mb, t)
            wmenushowConnId <- on wmenushow menuItemActivate $ windowToggle window
            on wmenuexit menuItemActivate $ widgetDestroy window

            -- window signals

            onDelete window $ \_ -> do
                noTray <- not <$> statusIconIsEmbedded wtray
                closePlease <- not <$> get wchecktray
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
            return (window, wchecktray)
        )
        (\(_, wct) c -> get wct <&> \a -> c{coTray=a})
        (\(w, _) e -> e{window=w})
    ,EP
        (\_ c -> setir ((fromIntegral . coMaxLines) c) =<< builderGetObject b castToSpinButton "wspinmaxlines")
        (\v c -> get v <&> \a -> c{coMaxLines=round a})
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

    wipeStarted <- newTVarIO False

    postCount <- newIORef 0
    wipeStats <- newIORef (0, 0, 0)
    captchasSolved <- newIORef 0

    proxies <- newIORef M.empty

    httpproxyMod <- newIORef nullTime
    httpproxyLast <- newIORef []

    socksproxyMod <- newIORef nullTime
    socksproxyLast <- newIORef []

    connection <- newManager def{managerConnCount = 1000000}

    threadGroup <- ThreadGroup.new

    (re, !rs) <- mfix $ \ ~(lolhaskell, _) -> do
        (setEnv, setConf) <- runEnvParts (envParts builder) lolhaskell conf
        return (setEnv E{..}, setConf)

    re `deepseq` return (re, rs)
