{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module GtkBlast.Conf
    (Conf(..)
    ,readConfig
    ,writeConfig
    ) where
import Import

import GtkBlast.Type_PastaSet
import GtkBlast.Type_CaptchaMode
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Directory

import BlastItWithPiss.Board

import Data.Version
import Paths_blast_it_with_piss

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import System.FilePath

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty

import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class

import GHC.Generics

import Text.ParserCombinators.ReadP

-- Fields are strict so it won't compile if anything is missing in Default or FromJSON instances
data Conf = Conf
    {coActiveBoards :: ![Board]
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
    ,coAntigateHost :: !String
    ,coLastVersion :: !Version
    ,coPastaFile :: !String
    ,coEscapeInv :: !Bool
    ,coEscapeWrd :: !Bool
    ,coPostAgitka :: !Bool
    ,coSortingByAlphabet :: !Bool
    ,coShuffleReposts :: !Bool
    ,coRandomQuote :: !Bool
    ,coUsePostTimeout :: !Bool
    ,coPostTimeout :: !Double
    ,coUseThreadTimeout :: !Bool
    ,coThreadTimeout :: !Double
    ,coUseFluctuation :: !Bool
    ,coFluctuation :: !Double
    ,coSage :: !Bool
    ,coMaxLines :: !Int
    }
  deriving (Eq, Show, Ord, Generic)

instance Default Conf where
    def = Conf
        {coActiveBoards = []
        ,coPastaSet = FromThread
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
        ,coSettingsShown = True
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
        ,coAntigateHost = "antigate.com"
        ,coLastVersion = version
        ,coPastaFile = bundledFile "pasta/shizik"
        ,coEscapeInv = False
        ,coEscapeWrd = False
        ,coPostAgitka = False
        ,coSortingByAlphabet = True
        ,coShuffleReposts = False
        ,coRandomQuote = False
        ,coUsePostTimeout = False
        ,coPostTimeout = ssachPostTimeout SsachB
        ,coUseThreadTimeout = False
        ,coThreadTimeout = ssachThreadTimeout SsachB
        ,coUseFluctuation = False
        ,coFluctuation = 10
        ,coSage = True
        ,coMaxLines = 300
        }

_parseWithDefault
    :: (FromJSON a, Show a)
    => Object -> Text -> a -> WriterT String Parser a
_parseWithDefault obj name _def = do
    x <- lift $ obj .:? name
    case x of
      Just v ->
        return v
      Nothing -> do
        tell $ "Couldn't parse field \"" ++ T.unpack name
            ++ "\", loading default value: " ++ show _def ++ "\n"
        return _def

-- snd contains warnings, we don't fail if some of the fields are missing.
instance FromJSON (Conf, String) where
    parseJSON (Object obj) = runWriterT $ do
-- CLARIFICATION this macro relies on -traditional or cpphs.
#define F(x) x <- _parseWithDefault obj "x" $ x def
        F(coActiveBoards)
        F(coPastaSet)
        F(coCreateThreads)
        F(coImageFolder)
        F(coAttachImages)
        F(coAnnoy)
        F(coHideOnSubmit)
        F(coAnnoyErrors)
        F(coTray)
        F(coWatermark)
        F(coSettingsShown)
        F(coAdditionalShown)
        F(coLogShown)
        F(coFirstLaunch)
        F(coUseHttpProxy)
        F(coHttpProxyFile)
        F(coUseSocksProxy)
        F(coSocksProxyFile)
        F(coUseNoProxy)
        F(coCaptchaMode)
        F(coAntigateKey)
        F(coAntigateHost)
        F(coLastVersion)
        F(coPastaFile)
        F(coEscapeInv)
        F(coEscapeWrd)
        F(coPostAgitka)
        F(coSortingByAlphabet)
        F(coShuffleReposts)
        F(coRandomQuote)
        F(coUsePostTimeout)
        F(coPostTimeout)
        F(coUseThreadTimeout)
        F(coThreadTimeout)
        F(coUseFluctuation)
        F(coFluctuation)
        F(coSage)
        F(coMaxLines)
#undef F
        return Conf{..}
    parseJSON _ = mzero

instance ToJSON Conf where

jsonReadInstance :: Read a => Value -> Parser a
jsonReadInstance (String s) = maybe mzero return $ readMay $ T.unpack s
jsonReadInstance _ = mzero

jsonShowInstance :: Show a => a -> Value
jsonShowInstance = String . T.pack . show

instance FromJSON Version where
    parseJSON (String s) = maybe mzero return $
        fst <$> lastMay (readP_to_S parseVersion $ T.unpack s)
    parseJSON _ = mzero

instance ToJSON Version where
    toJSON = String . T.pack . showVersion

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

readConfig :: FilePath -> IO Conf
readConfig configfile = do
    _x <- try $ B.readFile $ configfile
    case _x of
      Left (a::SomeException) -> do
        putInvisibleLog $ "Couldn't read config from \"" ++ configfile ++ "\" , loading defaults. Exception was: " ++ show a
        return def
      Right c' -> do
        let c = toLBS c'
        case decode' c of
          Nothing -> do
            let confbad = configfile <.> "old.faulty"
            putInvisibleLog $
                "Couldn't read config from \"" ++ configfile ++
                "\" because of syntax error, overwriting with defaults. " ++
                "Old version saved at \"" ++ confbad ++ "\""
            fromIOException (return ()) $ LB.writeFile confbad c
            return def
          Just (n, errs) -> do
            unless (null errs) $ putInvisibleLog errs
            return n

writeConfig :: FilePath -> Conf -> E ()
writeConfig configfile conf = do
    writeLog "Writing config"
    tw <- try $ io $ LB.writeFile configfile $ encodePretty conf
    case tw of
        Left (a::SomeException) ->
            writeLog $
                "Couldn't write config to \"" ++ configfile ++
                "\" , got exception: " ++ show a
        Right _ -> writeLog $ "Wrote config \"" ++ configfile ++"\": " ++ show conf