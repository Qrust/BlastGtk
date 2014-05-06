{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module GtkBlast.Conf
    (Conf(..)
    ,readConfig
    ,writeConfig
    ) where
import Import

import GtkBlast.Types
import GtkBlast.Environment
import GtkBlast.Log

import BlastItWithPiss.Board

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

-- Fields are strict so it won't compile if anything is missing in
-- Default or JSON instances
data Conf = Conf
    {coActiveBoards      :: ![Board]
    ,coPastaSet          :: !PastaSet
    ,coCreateThreads     :: !Bool
    ,coImageFolder       :: !String
    ,coAttachImages      :: !Bool
    ,coAnnoy             :: !Bool
    ,coHideOnSubmit      :: !Bool
    ,coAnnoyErrors       :: !Bool
    ,coTray              :: !Bool
    ,coWatermark         :: !Bool
    ,coFirstLaunch       :: !Bool
    ,coUseHttpProxy      :: !Bool
    ,coHttpProxyFile     :: !String
    ,coUseSocksProxy     :: !Bool
    ,coSocksProxyFile    :: !String
    ,coUseNoProxy        :: !Bool
    ,coCaptchaMode       :: !CaptchaMode
    ,coAntigateKey       :: !String
    ,coAntigateHost      :: !String
    ,coLastVersion       :: !GtkBlastVersion
    ,coPastaFile         :: !String
    ,coEscapeInv         :: !Bool
    ,coEscapeWrd         :: !Bool
    ,coSortingByAlphabet :: !Bool
    ,coShuffleReposts    :: !Bool
    ,coRandomQuote       :: !Bool
    ,coUsePostTimeout    :: !Bool
    ,coPostTimeout       :: !Double
    ,coUseThreadTimeout  :: !Bool
    ,coThreadTimeout     :: !Double
    ,coUseFluctuation    :: !Bool
    ,coFluctuation       :: !Double
    ,coSage              :: !Bool
    ,coMaxLines          :: !Int
    ,coPastaText         :: !Text
    ,coVideoSet          :: !VideoSet
    ,coVideoFile         :: !String
    ,coVideoText         :: !Text
    -- added in 1.2
    ,coPresolveCaptcha   :: !Bool
    ,coBoardSpeedData    :: ![(Board, Int)] -- fucking aeson
    ,coDomain            :: !String
    }
  deriving (Eq, Show, Ord, Generic)

_parseWithDefault
    :: (FromJSON a, Show a)
    => Object
    -> Text
    -> a -- ^ default value
    -- | either error message or value
    -> WriterT String Parser a
_parseWithDefault obj name _def = do
    x <- lift $ obj .:? name
    case x of
      Just v ->
        return v
      Nothing -> do
        tell $ "Couldn't parse field \"" ++ T.unpack name
            ++ "\", loading default value: " ++ show _def
            ++ "\n"
        return _def

-- | 'snd' contains warnings - we don't fail if some of the fields are missing.
instance Default Conf => FromJSON (Conf, String) where
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
        F(coPastaText)
        F(coVideoSet)
        F(coVideoText)
        F(coVideoFile)
        F(coPresolveCaptcha)
        F(coBoardSpeedData)
        F(coDomain)
#undef F
        return Conf{..}
    parseJSON _ = mzero

-- | Generic instance
instance ToJSON Conf where

instance FromJSON Board where
    parseJSON (String s) = maybe mzero return $ readBoard $ T.unpack s
    parseJSON _ = mzero

instance ToJSON Board where
    toJSON = String . renderBoard

readConfig :: Default Conf => FilePath -> IO Conf
readConfig configfile = do
    _x <- try $ B.readFile $ configfile
    case _x of
      Left (a::SomeException) -> do
        putInvisibleLog $
            "Couldn't read config from \"" ++ fromString configfile ++
            "\" , loading defaults. Exception was: " ++ show a
        return def
      Right _c -> do
        let c = toLBS _c
        case decode' c of
          Nothing -> do
            let confbad = configfile <.> "old.faulty"
            putInvisibleLog $
                "Couldn't read config from \"" ++ fromString configfile ++
                "\" because of syntax error, overwriting with defaults. " ++
                "Old version saved at \"" ++ fromString confbad ++ "\""
            fromIOException (return ()) $ LB.writeFile confbad c
            return def
          Just (n, errs) -> do
            unless (null errs) $ putInvisibleLog $ fromString errs
            return n

writeConfig :: FilePath -> Conf -> E ()
writeConfig configfile conf = do
    writeLog "Writing config"
    tw <- try $ io $ LB.writeFile configfile $ encodePretty conf
    case tw of
        Left (a::SomeException) ->
            writeLog $
                "Couldn't write config to \"" ++ fromString configfile
             ++ "\" , got exception: " ++ show a
        Right _ ->
            writeLog $
                "Wrote config \"" ++ fromString configfile
             ++ "\": " ++ show conf
