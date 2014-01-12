module GtkBlast.GuiCaptcha
    (addGuiCaptchas
    ,addGuiCaptcha
    ,killGuiCaptcha
    ,deactivateGuiCaptcha
    ,guiCaptchaEnvPart
    ,maintainGuiCaptcha
    ) where
import Import hiding (on, mod)

import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.EnvPart

import BlastItWithPiss
import BlastItWithPiss.Board
import BlastItWithPiss.Blast

import System.IO.Temp

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Graphics.UI.Gtk hiding (get, set)

captchaError :: Text -> E ()
captchaError =
    showMessage wcheckannoy "Captcha error" (Just 2) True

captchaCaptchaRequestPersistent :: Text -> E ()
captchaCaptchaRequestPersistent s =
    showMessage wcheckannoy "Persistent captcha message" Nothing False s
    -- showCaptchaRequest increments messageLocks
    -- we deincrement messageLock in removeCurrentCaptcha

{-# INLINABLE formatCaptchaCaptchaRequest #-}
formatCaptchaCaptchaRequest :: CaptchaType -> CaptchaOrigin -> Text
formatCaptchaCaptchaRequest CaptchaPosting (AgentCaptcha st) =
    "Введите капчу для "
     ++ (case oThread st of
          Nothing -> "создания нового треда " ++ renderCompactStamp st
          t -> "Поста в тред " ++ ssachThread (oBoard st) t)
            ++ maybeNoProxy "" (\p -> " с прокси {" ++ show p ++ "}") (oProxy st)
formatCaptchaCaptchaRequest CaptchaCloudflare (AgentCaptcha st) =
    "Введите капчу Cloudflare для " ++ show (oProxy st)
formatCaptchaCaptchaRequest _ PresolverCaptcha =
    "Введите капчу для сбора капчи"

updateCaptchaWidget :: E ()
updateCaptchaWidget = do
    E{ pendingGuiCaptchas
     , wimagecaptcha
     , wentrycaptcha } <- ask
    pc <- io $ readIORef pendingGuiCaptchas
    case pc of
        [] -> tempError 2 $ "Switching while there are no captchas."
        ((st, c):_) -> control $ \lio ->
            withSystemTempFile "captcha.image" $ \fn h -> lio $ do
                io $ L.hPut h $ captchaBytes c
                io $ hClose h
                io $ imageSetFromFile wimagecaptcha fn
                writeLog "switched captcha"
                io $ entrySetText wentrycaptcha ""
                captchaCaptchaRequestPersistent $
                    formatCaptchaCaptchaRequest (captchaType c) st

putCaptchaWidget :: E ()
putCaptchaWidget = do
    E{ wprogressalignment
     , wprogresswipe
     , wvboxcaptcha
     , wentrycaptcha
     , wbuttoncaptchaok } <- ask
    io $ do
        containerRemove wprogressalignment wprogresswipe
        containerAdd wprogressalignment wvboxcaptcha
        widgetGrabFocus wentrycaptcha
        widgetGrabDefault wbuttoncaptchaok

removeCaptchaWidget :: E ()
removeCaptchaWidget = do
    E{ wprogressalignment
     , wvboxcaptcha
     , wprogresswipe
     , wcheckhideonsubmit
     , window } <- ask
    io $ do
        containerRemove wprogressalignment wvboxcaptcha
        containerAdd wprogressalignment wprogresswipe
        whenM (get wcheckhideonsubmit) $
            widgetHide window

addGuiCaptchas :: [(CaptchaOrigin, CaptchaRequest)] -> E ()
addGuiCaptchas [] =
    writeLog "Added 0 gui captchas..."
addGuiCaptchas sps = do
    E{ pendingGuiCaptchas } <- ask
    pc <- get pendingGuiCaptchas
    mod pendingGuiCaptchas (++sps)
    writeLog $ "Added " ++ show (length sps) ++ " gui captchas"
    when (null pc) $ do
        updateCaptchaWidget
        putCaptchaWidget

addGuiCaptcha :: (CaptchaOrigin, CaptchaRequest) -> E ()
addGuiCaptcha sp = addGuiCaptchas [sp]

resetCurrentCaptcha :: E ()
resetCurrentCaptcha = do
    E{ messageLocks
     , pendingGuiCaptchas } <- ask
    mod messageLocks (subtract 1)
    ifM (null <$> get pendingGuiCaptchas)
        removeCaptchaWidget
        updateCaptchaWidget

removeCurrentCaptchaWith :: ((CaptchaOrigin, CaptchaRequest) -> E ()) -> E ()
removeCurrentCaptchaWith f = do
    E{ pendingGuiCaptchas } <- ask
    pc <- get pendingGuiCaptchas
    case pc of
        [] -> tempError 2 $
            "Tried to remove current captcha when there are no pending captchas"
        (c:cs) -> do
            f c
            set pendingGuiCaptchas cs
            resetCurrentCaptcha

sendCurrentCaptcha :: CaptchaAnswer -> E ()
sendCurrentCaptcha a = do
    removeCurrentCaptchaWith $ \(st, c) -> do
        writeLog $ "Sending " ++ show a ++ " to " ++
            renderCaptchaOrigin st
        io $ captchaSend c a

maintainGuiCaptcha :: [(Board, [BlastProxy])] -> E ()
maintainGuiCaptcha blacklist = do
    E{ pendingGuiCaptchas
     , proxies } <- ask

    pgc <- get pendingGuiCaptchas
    unless (null pgc) $ do
        pxs <- get proxies

        let
          (good, bad) = (`partition` pgc) $
            \(est, _) -> case est of
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

        -- oh dog what done.
        set pendingGuiCaptchas good

        -- clear all the bad captchas, we imply that 'captchaSend's won't block
        -- and are not malicious (true for "BlastItWithPiss.CaptchaServer"s)
        forM_ bad $ \(est, captchaReq) -> do
            writeLog $ "Aborting captcha for blacklisted agent: " ++
                renderCaptchaOrigin est
            io $ captchaSend captchaReq AbortCaptcha

        unless (null bad) $ do
            writeLog $
                "Filtered gui captcha from dead proxies, bad captchas: "
              ++ show (length bad)

        -- if any blacklisted captchas share similarities with the current HEAD
        -- captcha, it must mean that the current HEAD is itself blacklisted!
        -- therefore we delete most recent captch, and reset the widget
        let (headStamp',_) = head pgc
        when ((`any` bad) $ \(badStamp',_) ->
            case headStamp' of
              AgentCaptcha headStamp -> case badStamp' of
                AgentCaptcha badStamp ->
                    oBoard badStamp == oBoard headStamp
                 && oProxy badStamp == oProxy headStamp
                _ -> False
              _ -> False) $ do
            writeLog $ "Resetting captcha widget, because owner of the current "
                ++ " captcha is blacklisted!"
            resetCurrentCaptcha

-- | Should only be called when you're sure no one blocks on captcha
killGuiCaptcha :: E ()
killGuiCaptcha = do
    E{ messageLocks
     , pendingGuiCaptchas } <- ask
    writeLog "Killing gui captcha"
    pgc <- get pendingGuiCaptchas
    when (not $ null pgc) removeCaptchaWidget
    set pendingGuiCaptchas []
    mod messageLocks (subtract $ length pgc)

deactivateGuiCaptcha :: E [(CaptchaOrigin, CaptchaRequest)]
deactivateGuiCaptcha = do
    E{ pendingGuiCaptchas } <- ask
    writeLog "Deactivating gui captcha..."
    oldPc <- get pendingGuiCaptchas
    replicateM_ (length oldPc) $ removeCurrentCaptchaWith $
        \_ -> writeLog "Removing gui captcha"
    return oldPc

guiCaptchaEnvPart :: Builder -> EnvPart
guiCaptchaEnvPart b = EP
    (\e _ -> do
        wvboxcaptcha <- builderGetObject b castToVBox "vboxcaptcha"
        weventboxcaptcha <- builderGetObject b castToEventBox "eventboxcaptcha"
        wimagecaptcha <- builderGetObject b castToImage "imagecaptcha"
        wentrycaptcha <- builderGetObject b castToEntry "entrycaptcha"
        wbuttoncaptchaok <- builderGetObject b castToButton "buttoncaptchaok"
        wbuttoncaptchacancel <- builderGetObject b castToButton "buttoncaptchacancel"

        pendingGuiCaptchas <- newIORef []

        void $ on weventboxcaptcha buttonPressEvent $ do
            io $ runE e $ sendCurrentCaptcha ReloadCaptcha
            return True

        void $ on wbuttoncaptchacancel buttonActivated $ do
            runE e $ sendCurrentCaptcha AbortCaptcha

        void $ on wbuttoncaptchaok buttonActivated $ do
            x <- entryGetText wentrycaptcha
            {-if null x
                then captchaError "Пожалуйста введите капчу"
                else removeCurrentCaptcha $ Answer x-}
            runE e $ sendCurrentCaptcha $ Answer x $
                \st -> postGUIAsync $ runE e $
                    captchaError $ "Неправильно введена капча\n"
                      ++ renderCompactStamp st
            atomicModifyIORef' (captchasSolved e) $ \a -> (a + 1, ())

        return (wvboxcaptcha, wimagecaptcha, wentrycaptcha, wbuttoncaptchaok, pendingGuiCaptchas))
    (const return)
    (\(wvc, wic, wec, wbco, pc) e ->
        e{wvboxcaptcha=wvc
         ,wimagecaptcha=wic
         ,wentrycaptcha=wec
         ,wbuttoncaptchaok=wbco
         ,pendingGuiCaptchas=pc
         })
