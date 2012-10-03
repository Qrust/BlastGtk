{-# LANGUAGE NoImplicitPrelude #-}
module GtkBlast.Conf
    (Conf(..)
    ,readConfig
    ,writeConfig
    ) where
import Import
import GtkBlast.IO
import GtkBlast.Type_PastaSet
import GtkBlast.Type_CaptchaMode
import GtkBlast.Environment
import GtkBlast.Log
import "blast-it-with-piss" BlastItWithPiss.Board
import System.FilePath

data Conf = Conf {coActiveBoards :: [Board]
                 ,coPastaSet :: PastaSet
                 ,coCreateThreads :: Bool
                 ,coImageFolder :: String
                 ,coAttachImages :: Bool
                 ,coAnnoy :: Bool
                 ,coHideOnSubmit :: Bool
                 ,coAnnoyErrors :: Bool
                 ,coTray :: Bool
                 ,coWatermark :: Bool
                 ,coSettingsShown :: Bool
                 ,coAdditionalShown :: Bool
                 ,coLogShown :: Bool
                 ,coFirstLaunch :: Bool
                 ,coUseHttpProxy :: Bool
                 ,coHttpProxyFile :: String
                 ,coUseSocksProxy :: Bool
                 ,coSocksProxyFile :: String
                 ,coUseNoProxy :: Bool
                 ,coCaptchaMode :: CaptchaMode
                 ,coAntigateKey :: String
                 }
    deriving (Eq, Show, Read)

instance Default Conf where
    def = Conf
         { -- FIXME coActiveBoards = [B, BB, ABU, D, VG, PR, DEV]
          coActiveBoards = [NE, MDK]
         ,coPastaSet = Mocha
         ,coCreateThreads = True
         ,coImageFolder = "images"
         ,coAttachImages = True
         ,coAnnoy = True
         ,coHideOnSubmit = False
         ,coAnnoyErrors = True
#ifdef TEST
         ,coTray = False
#else
         ,coTray = True
#endif
         ,coWatermark = False
         ,coSettingsShown = False
         ,coAdditionalShown = False
         ,coLogShown = False
         ,coFirstLaunch = True
         ,coUseHttpProxy = False
         ,coHttpProxyFile = ""
         ,coUseSocksProxy = False
         ,coSocksProxyFile = ""
         ,coUseNoProxy = True
         ,coCaptchaMode = Gui
         ,coAntigateKey = []
         }

readConfig :: FilePath -> IO Conf
readConfig configfile = do
    x <- try $ readFile $ configfile
    case x of
        Left (a::SomeException) -> do
            rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" , loading defaults. Exception was: " ++ show a
            return def
        Right c ->
            case readMay c of
                Nothing -> do
                    let confold = configfile <.> "old.faulty"
                    rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" because of syntax error, overwriting with defaults. Old version saved at \"" ++ confold ++ "\""
                    fromIOEM (return ()) $
                        writeFile confold c
                    return def
                Just n -> return n

writeConfig :: FilePath -> Conf -> E ()
writeConfig configfile conf = do
    tw <- try $ io $ writeFile configfile $ show conf
    case tw of
        Left (a::SomeException) -> writeLog $ "Couldn't write config to \"" ++ configfile ++ "\" , got exception: " ++ show a
        Right _ -> writeLog $ "Wrote config \"" ++ configfile ++"\": " ++ show conf
