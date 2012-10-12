{-# LANGUAGE DeriveGeneric #-}
module Updater.Manifest
    (URL
    ,MD5Sum
    ,Platform(..)
    ,Changelog
    ,UpdateManifest(..)
    ,Outcome(..)
    ) where
import Prelude
import Safe
import Control.Applicative
import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Version
import Control.Exception
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

type URL = String

type MD5Sum = String

type Changelog = [(Version, String)]

data Platform = Linux | Windows | Mac
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

data UpdateManifest = UpdateManifest
        {version :: !Version
        ,binaryAndResourcesZipArchives :: ![(URL, MD5Sum)]
        ,imagePackZipArchives :: ![(URL, MD5Sum)]
        ,changelog :: !Changelog
        }
    deriving (Eq, Show, Generic)

data Outcome = Success
             | ChecksumMismatch
             | ServersUnreachable
             | NoBuildAvailable Platform
             | UnparseableManifest
             | ConnException SomeException
    deriving (Show)

-- HACK Those are quite dangerous orphans
instance FromJSON Version where
    parseJSON (String s) = maybe mzero return $
        fst <$> lastMay (readP_to_S parseVersion $ T.unpack s)
    parseJSON _ = mzero

instance ToJSON Version where
    toJSON = String . T.pack . showVersion
-- /HACK

instance FromJSON Platform
instance ToJSON Platform

instance FromJSON UpdateManifest where
    parseJSON (Object o) = do
        version <- o .: "version"
        binaryAndResourcesZipArchives <- o .: "binaryAndResourcesZipArchives"
        imagePackZipArchives <- o .:? "imagePackZipArchives" .!= []
        changelog <- o .:? "changelog" .!= []
        return UpdateManifest{..}
    parseJSON _ = mzero

instance ToJSON UpdateManifest
