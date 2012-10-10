module GtkBlast.GuiCaptcha
    (addGuiCaptchas
    ,addGuiCaptcha
    ,killGuiCaptcha
    ,deactivateGuiCaptcha
    ,guiCaptchaEnvPart
    ,maintainGuiCaptcha
    ) where
import Import hiding (on, mod)
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.EnvPart
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Board
import "blast-it-with-piss" BlastItWithPiss.Blast
import Graphics.UI.Gtk hiding (get, set)
import System.IO.Temp
import qualified Data.ByteString.Lazy as L

captchaMessage :: String -> E ()
captchaMessage s =
    showMessage wcheckannoy "Captcha message" Nothing False s
    -- showMessage increments messageLocks, we deincrement messageLock in removeCurrentCaptcha

formatCaptchaMessage :: CaptchaType -> OriginStamp -> String
formatCaptchaMessage CaptchaPosting (OriginStamp _ proxy board _ thread) =
    "Введите капчу для " ++
        (case thread of
            Nothing -> "создания нового треда в " ++ renderBoard board
            t -> "Поста в тред " ++ ssachThread board t) ++
                maybeNoProxy "" (\p -> "с прокси {" ++ show p ++ "}") proxy
formatCaptchaMessage CaptchaCloudflare (OriginStamp _ proxy _ _ _) =
    "Введите капчу Cloudflare для " ++ show proxy

updateCaptchaWidget :: E ()
updateCaptchaWidget = do
    E{..} <- ask
    pc <- io $ readIORef pendingGuiCaptchas
    case pc of
        [] -> tempError 2 $ "Switching while there are no captchas."
        ((st, c):_) -> control $ \lio ->
            withSystemTempFile "recaptcha-captcha-image.jpeg" $ \fn h -> lio $ do
                io $ L.hPut h $ captchaBytes c
                io $ hClose h
                io $ imageSetFromFile wimagecaptcha fn
                writeLog "switched captcha"
                io $ entrySetText wentrycaptcha ""
                captchaMessage $ formatCaptchaMessage (captchaType c) st

putCaptchaWidget :: E ()
putCaptchaWidget = do
    E{..} <- ask
    io $ do
        containerRemove wprogressalignment wprogresswipe
        containerAdd wprogressalignment wvboxcaptcha
        widgetGrabFocus wentrycaptcha
        widgetGrabDefault wbuttoncaptchaok

removeCaptchaWidget :: E ()
removeCaptchaWidget = do
    E{..} <- ask
    io $ do
        containerRemove wprogressalignment wvboxcaptcha
        containerAdd wprogressalignment wprogresswipe
        whenM (get wcheckhideonsubmit) $
            widgetHide window

addGuiCaptchas :: [(OriginStamp, Message)] -> E ()
addGuiCaptchas [] = writeLog "Added 0 gui captchas..."
addGuiCaptchas sps = do
    E{..} <- ask
    pc <- get pendingGuiCaptchas
    mod pendingGuiCaptchas (++sps)
    when (null pc) $ do
        updateCaptchaWidget
        putCaptchaWidget

addGuiCaptcha :: (OriginStamp, Message) -> E ()
addGuiCaptcha sp = addGuiCaptchas [sp]

removeCurrentCaptchaWith :: ((OriginStamp, Message) -> E ()) -> E ()
removeCurrentCaptchaWith f = do
    E{..} <- ask
    pc <- get pendingGuiCaptchas
    case pc of
        [] -> tempError 2 $ "Tried to remove current captcha when there are no pending captchas"
        (c:cs) -> do
            f c
            set pendingGuiCaptchas cs
            mod messageLocks (subtract 1)
            if (null cs)
                then removeCaptchaWidget
                else updateCaptchaWidget

removeCurrentCaptcha :: CaptchaAnswer -> E ()
removeCurrentCaptcha a = do
    removeCurrentCaptchaWith $ \(OriginStamp{..}, c) -> do
        writeLog $ "Sending " ++ show a ++ " to " ++ "{" ++ show oProxy ++ "} " ++ renderBoard oBoard
        io $ captchaSend c a

killGuiCaptcha :: E ()
killGuiCaptcha = do
    E{..} <- ask
    writeLog "Killing gui captcha"
    whenM (not . null <$> get pendingGuiCaptchas) $
        removeCaptchaWidget
    set pendingGuiCaptchas []

deactivateGuiCaptcha :: E [(OriginStamp, Message)]
deactivateGuiCaptcha = do
    E{..} <- ask
    writeLog "Deactivating gui captcha..."
    oldPc <- get pendingGuiCaptchas
    replicateM_ (length oldPc) $ removeCurrentCaptchaWith $ const $
        writeLog "Removing gui captcha"
    return oldPc

guiCaptchaEnvPart :: Builder -> EnvPart
guiCaptchaEnvPart b = EP
    (\env _ -> do
        wvboxcaptcha <- builderGetObject b castToVBox "vboxcaptcha"
        weventboxcaptcha <- builderGetObject b castToEventBox "eventboxcaptcha"
        wimagecaptcha <- builderGetObject b castToImage "imagecaptcha"
        wentrycaptcha <- builderGetObject b castToEntry "entrycaptcha"
        wbuttoncaptchaok <- builderGetObject b castToButton "buttoncaptchaok"
        wbuttoncaptchacancel <- builderGetObject b castToButton "buttoncaptchacancel"

        pendingGuiCaptchas <- newIORef []

        void $ on weventboxcaptcha buttonPressEvent $ do
            io $ runE env $ removeCurrentCaptcha ReloadCaptcha
            return True
    
        void $ on wbuttoncaptchaok buttonActivated $ do
            x <- entryGetText wentrycaptcha
            {-if null x
                then captchaMessage "Пожалуйста введите капчу"
                else removeCurrentCaptcha $ Answer x-}
            runE env $ removeCurrentCaptcha $ Answer x
    
        void $ on wbuttoncaptchacancel buttonActivated $ do
            runE env $ removeCurrentCaptcha AbortCaptcha

        return (wvboxcaptcha, wimagecaptcha, wentrycaptcha, wbuttoncaptchaok, pendingGuiCaptchas))
    (const return)
    (\(wvc, wic, wec, wbco, pc) e -> e{wvboxcaptcha=wvc
                                      ,wimagecaptcha=wic
                                      ,wentrycaptcha=wec
                                      ,wbuttoncaptchaok=wbco
                                      ,pendingGuiCaptchas=pc})

maintainGuiCaptcha :: E ()
maintainGuiCaptcha = return ()
