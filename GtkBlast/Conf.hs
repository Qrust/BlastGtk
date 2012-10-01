{-# LANGUAGE NoImplicitPrelude #-}
module GtkBlast.Conf
    (Conf(..)
    ,writeConfig
    ) where
import Import
import GtkBlast.IO
import GtkBlast.Pasta
import GtkBlast.Environment
import GtkBlast.Log
import "blast-it-with-piss" BlastItWithPiss.Board

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
         }

writeConfig :: FilePath -> (Conf -> IO Conf) -> E ()
writeConfig configfile setConf = do
    nconf <- io $ setConf def{coFirstLaunch=False}

    tw <- try $ io $ writeFile configfile $ show nconf
    case tw of
        Left (a::SomeException) -> writeLog $ "Couldn't write config to \"" ++ configfile ++ "\" , got exception: " ++ show a
        Right _ -> writeLog $ "Wrote config \"" ++ configfile ++"\": " ++ show nconf
