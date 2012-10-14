{-# LANGUAGE DeriveGeneric #-}
module Updater.Manifest
    (URL
    ,MD5Sum
    ,Platform(..)
    ,Changelog
    ,UpdateManifest(..)
    ,renderMD5
    ) where
import Prelude
import Safe
import Control.Applicative
import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Version
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Crypto.Hash.MD5 (MD5)
import qualified Data.Serialize as S
import qualified Data.ByteString as B
import Numeric

type URL = String

type MD5Sum = String

type Changelog = [(Version, String)]

data Platform = Linux | Windows | Mac
    deriving (Eq, Show, Read, Ord, Enum, Bounded)

data UpdateManifest = UpdateManifest
        {version :: !Version
        ,binaryAndResourcesZipArchives :: ![(Platform, (URL, MD5Sum))]
        ,imagePackZipArchives :: ![(String, (URL, MD5Sum))]
        ,changelog :: !Changelog
        }
    deriving (Eq, Show, Generic)

-- HACK Those are quite dangerous orphans
instance FromJSON Version where
    parseJSON (String s) = maybe mzero return $
        fst <$> lastMay (readP_to_S parseVersion $ T.unpack s)
    parseJSON _ = mzero

instance ToJSON Version where
    toJSON = String . T.pack . showVersion
-- /HACK

instance FromJSON Platform where
    parseJSON (String s) = maybe mzero return $ readMay $ T.unpack s
    parseJSON _ = mzero

instance ToJSON Platform where
    toJSON = String . T.pack . show

instance FromJSON UpdateManifest where
    parseJSON (Object o) = do
        version <- o .: "version"
        binaryAndResourcesZipArchives <- o .: "binaryAndResourcesZipArchives"
        imagePackZipArchives <- o .:? "imagePackZipArchives" .!= []
        changelog <- o .:? "changelog" .!= []
        return UpdateManifest{..}
    parseJSON _ = mzero

instance ToJSON UpdateManifest

--seriously?
renderMD5 :: MD5 -> MD5Sum
renderMD5 md5 = concatMap shWord8 $ B.unpack $ S.encode md5
  where add0 (x:[]) = '0':x:[]
        add0 x = x
        shWord8 x = add0 $ showHex x ""
