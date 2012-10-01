{-# LANGUAGE NoImplicitPrelude #-}
module GtkBlast.GuiCaptcha
    (activateCaptcha
    ,addCaptcha
    ,removeCaptcha
    ) where
import Import
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Log
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Board
import "blast-it-with-piss" BlastItWithPiss.Blast
import Graphics.UI.Gtk hiding (get)
import System.IO.Temp
import qualified Data.ByteString.Lazy as L

formatCaptchaMessage :: CaptchaType -> OriginStamp -> String
formatCaptchaMessage CaptchaPosting (OriginStamp _ proxy board _ thread) =
    "Введите капчу для " ++
        (case thread of
            Nothing -> "создания нового треда в " ++ renderBoard board
            Just t -> "Поста в тред " ++ ssachThread board t) ++
                maybeNoProxy "" (\p -> "с прокси {" ++ show p ++ "}") proxy
formatCaptchaMessage CaptchaCloudflare (OriginStamp _ proxy _ _ _) =
    "Введите капчу Cloudflare для " ++ show proxy

activateCaptcha :: E ()
activateCaptcha = do
    E{..} <- ask
    pc <- io $ readIORef pendingCaptchas
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

addCaptcha :: (OriginStamp, Message) -> E ()
addCaptcha sp = do
    E{..} <- ask
    pc <- io $ readIORef pendingCaptchas
    io $ modifyIORef pendingCaptchas (++[sp])
    when (null pc) $ do
        io $ do
            containerRemove wprogressalignment wprogresswipe
            containerAdd wprogressalignment wvboxcaptcha
            widgetGrabFocus wentrycaptcha
            widgetGrabDefault wbuttoncaptchaok
        activateCaptcha

removeCaptcha :: CaptchaAnswer -> E ()
removeCaptcha a = do
    E{..} <- ask
    cs <- io $ readIORef pendingCaptchas
    case cs of
        [] -> tempError 2 $ "Ответил на несуществующий запрос капчи"
        ((_,c):n) -> do
            writeLog $ "Sending " ++ show a ++ " to captcha requester"
            io $ captchaSend c a
            io $ writeIORef pendingCaptchas n
            if (null n)
                then io $ do
                    containerRemove wprogressalignment wvboxcaptcha
                    containerAdd wprogressalignment wprogresswipe
                    whenM (get wcheckhideonsubmit) $
                        widgetHide window
                else activateCaptcha
            io $ modifyIORef messageLocks (subtract 1)
