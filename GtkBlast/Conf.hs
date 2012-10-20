{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Version
import System.FilePath
import Paths_blast_it_with_piss
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import Text.ParserCombinators.ReadP
import GHC.Generics
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

-- Fields are made strict so it won't compile if anything is missing in default or fromjson instance
data Conf = Conf {coActiveBoards :: ![Board]
                 ,coPastaSet :: !PastaSet
                 ,coCreateThreads :: !Bool
                 ,coImageFolder :: !String
                 ,coAttachImages :: !Bool
                 ,coAnnoy :: !Bool
                 ,coHideOnSubmit :: !Bool
                 ,coAnnoyErrors :: !Bool
                 ,coTray :: !Bool
                 ,coWatermark :: !Bool
                 ,coSettingsShown :: !Bool
                 ,coAdditionalShown :: !Bool
                 ,coLogShown :: !Bool
                 ,coFirstLaunch :: !Bool
                 ,coUseHttpProxy :: !Bool
                 ,coHttpProxyFile :: !String
                 ,coUseSocksProxy :: !Bool
                 ,coSocksProxyFile :: !String
                 ,coUseNoProxy :: !Bool
                 ,coCaptchaMode :: !CaptchaMode
                 ,coAntigateKey :: !String
                 ,coLastVersion :: !Version
                 ,coPastaFile :: !String
                 ,coEscapeInv :: !Bool
                 ,coEscapeWrd :: !Bool
                 }
    deriving (Eq, Show, Ord, Generic)

instance Default Conf where
    def = Conf
         { -- FIXME coActiveBoards = [B, BB, ABU, D, VG, PR, DEV]
           -- coActiveBoards = [NE, MDK]
          coActiveBoards = [B, VG]
         ,coPastaSet = PastaFile
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
         ,coLastVersion = version
         ,coPastaFile = "resources/mocha"
         ,coEscapeInv = True
         ,coEscapeWrd = True
         }

-- HACK Those are quite dangerous orphans
instance FromJSON Version where
    parseJSON (String s) = maybe mzero return $
        fst <$> lastMay (readP_to_S parseVersion $ T.unpack s)
    parseJSON _ = mzero

instance ToJSON Version where
    toJSON = String . T.pack . showVersion
-- /HACK

jsonReadInstance :: Read a => Value -> Parser a
jsonReadInstance (String s) = maybe mzero return $ readMay $ T.unpack s
jsonReadInstance _ = mzero

jsonShowInstance :: Show a => a -> Value
jsonShowInstance = String . T.pack . show

instance FromJSON CaptchaMode where
    parseJSON = jsonReadInstance

instance ToJSON CaptchaMode where
    toJSON = jsonShowInstance

instance FromJSON PastaSet where
    parseJSON = jsonReadInstance

instance ToJSON PastaSet where
    toJSON = jsonShowInstance

instance FromJSON Board where
    parseJSON (String s) = maybe mzero return $ readBoard $ T.unpack s
    parseJSON _ = mzero

instance ToJSON Board where
    toJSON = String . renderBoard

-- snd contains warnings, we don't fail if some of the fields are missing.
instance FromJSON (Conf, String) where
    parseJSON (Object obj) = runWriterT $ do
        let f name getDef = do
                x <- lift $ obj .:? name
                case x of
                    Just v -> return v
                    Nothing -> do
                        let v = getDef def
                        v <$ tell ("Couldn't parse field \"" ++ T.unpack name ++ "\", loading default value: " ++ show v ++ "\n")
        coActiveBoards <- f "coActiveBoards" coActiveBoards
        coPastaSet <- f "coPastaSet" coPastaSet
        coCreateThreads <- f "coCreateThreads" coCreateThreads
        coImageFolder <- f "coImageFolder" coImageFolder
        coAttachImages <- f "coAttachImages" coAttachImages
        coAnnoy <- f "coAnnoy" coAnnoy
        coHideOnSubmit <- f "coHideOnSubmit" coHideOnSubmit
        coAnnoyErrors <- f "coAnnoyErrors" coAnnoyErrors
        coTray <- f "coTray" coTray
        coWatermark <- f "coWatermark" coWatermark
        coSettingsShown <- f "coSettingsShown" coSettingsShown
        coAdditionalShown <- f "coAdditionalShown" coAdditionalShown
        coLogShown <- f "coLogShown" coLogShown
        coFirstLaunch <- f "coFirstLaunch" coFirstLaunch
        coUseHttpProxy <- f "coUseHttpProxy" coUseHttpProxy
        coHttpProxyFile <- f "coHttpProxyFile" coHttpProxyFile
        coUseSocksProxy <- f "coUseSocksProxy" coUseSocksProxy
        coSocksProxyFile <- f "coSocksProxyFile" coSocksProxyFile
        coUseNoProxy <- f "coUseNoProxy" coUseNoProxy
        coCaptchaMode <- f "coCaptchaMode" coCaptchaMode
        coAntigateKey <- f "coAntigateKey" coAntigateKey
        coLastVersion <- f "coLastVersion" coLastVersion
        coPastaFile <- f "coPastaFile" coPastaFile
        coEscapeInv <- f "coEscapeInv" coEscapeInv
        coEscapeWrd <- f "coEscapeWrd" coEscapeWrd
        return Conf{..}
    parseJSON _ = mzero

-- CLARIFICATION (Un)fortunately, we can't use TH due to some obscure linking errors on winDOS.
instance ToJSON Conf where

readConfig :: FilePath -> IO Conf
readConfig configfile = do
    x <- try $ B.readFile $ configfile
    case x of
        Left (a::SomeException) -> do
            rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" , loading defaults. Exception was: " ++ show a
            return def
        Right c' -> do
            let c = toLBS c'
            case decode' c of
                Nothing -> do
                    let confold = configfile <.> "old.faulty"
                    rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" because of syntax error, overwriting with defaults. Old version saved at \"" ++ confold ++ "\""
                    fromIOEM (return ()) $
                        LB.writeFile confold c
                    return def
                Just (n, errs) -> do
                    when (not $ null errs) $ rawPutLog errs
                    return n

writeConfig :: FilePath -> Conf -> E ()
writeConfig configfile conf = do
    tw <- try $ io $ LB.writeFile configfile $ encodePretty conf
    case tw of
        Left (a::SomeException) -> writeLog $ "Couldn't write config to \"" ++ configfile ++ "\" , got exception: " ++ show a
        Right _ -> writeLog $ "Wrote config \"" ++ configfile ++"\": " ++ show conf
