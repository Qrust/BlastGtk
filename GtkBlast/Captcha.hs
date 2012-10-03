{-# LANGUAGE NoImplicitPrelude #-}
module GtkBlast.Captcha
    (CaptchaMode(..)
    ,addCaptcha
    ,killAllCaptcha
    ,captchaModeEnvPart
    ,maintainCaptcha
    ) where
import Import hiding (on, mod)
import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Conf
import GtkBlast.EnvPart
import GtkBlast.GuiCaptcha
import GtkBlast.AntigateCaptcha
import GtkBlast.Type_CaptchaMode
import "blast-it-with-piss" BlastItWithPiss
import Graphics.UI.Gtk hiding (get, set)

cmToBool :: CaptchaMode -> Bool
cmToBool Gui = False
cmToBool Antigate = True

cmFromBool :: Bool -> CaptchaMode
cmFromBool False = Gui
cmFromBool True = Antigate

addCaptcha :: (OriginStamp, Message) -> E ()
addCaptcha sp = do
    cm <- get =<< asks captchaMode
    case cm of
        Gui -> addGuiCaptcha sp
        Antigate -> addAntigateCaptcha sp

killAllCaptcha :: E ()
killAllCaptcha = do
    cm <- get =<< asks captchaMode
    case cm of
        Gui -> killGuiCaptcha
        Antigate -> killAntigateCaptcha

deactivateCaptcha :: CaptchaMode -> E [(OriginStamp, Message)]
deactivateCaptcha Gui = deactivateGuiCaptcha
deactivateCaptcha Antigate = deactivateAntigateCaptcha

addCaptchas :: CaptchaMode -> [(OriginStamp, Message)] -> E ()
addCaptchas Gui = addGuiCaptchas
addCaptchas Antigate = addAntigateCaptchas

migrateCaptcha :: CaptchaMode -> CaptchaMode -> E ()
migrateCaptcha ocm ncm = do
    writeLog $ "Migrating captchas from " ++ show ocm ++ " to " ++ show ncm
    captchas <- deactivateCaptcha ocm
    writeLog $ "Got " ++ show (length captchas) ++ " for migration"
    addCaptchas ncm captchas

captchaModeEnvPart :: Builder -> EnvPart
captchaModeEnvPart b = EP
    (\e c -> do
        wcheckantigate <- (rec (cmToBool . coCaptchaMode) $ builderGetObject b castToCheckButton "checkantigate") e c

        captchaMode <- newIORef (coCaptchaMode c)

        void $ on wcheckantigate buttonActivated $ do
            oldMode <- get captchaMode
            let newMode = cmFromBool $ not $ cmToBool oldMode
            set captchaMode newMode
            runE e $ migrateCaptcha oldMode newMode

        return captchaMode
    )
    (\v c -> get v ? \a -> c{coCaptchaMode=a})
    (\v e -> e{captchaMode=v})

maintainCaptcha :: E ()
maintainCaptcha = do
    cm <- get =<< asks captchaMode
    case cm of
        Gui -> maintainGuiCaptcha
        Antigate -> maintainAntigateCaptcha