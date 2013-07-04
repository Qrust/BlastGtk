module GtkBlast.Types
    (CaptchaMode(..)
    ,PastaSet(..)
    ,VideoSet(..)
    ,GtkBlastVersion(..)
    ) where
import Import
import Data.Ix

import qualified Data.Text as T

import GtkBlast.JsonUtils

import Data.Aeson

import Data.Version

import qualified Text.Show as Show
import Text.ParserCombinators.ReadP

data CaptchaMode
    = Gui
    | Antigate
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Ix)

data PastaSet
    = PastaFile
    | Symbol
    | FromThread
    | NoPasta
    | FromWidget
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Ix)

data VideoSet
    = VideoNothing
    | VideoFromFile
    | VideoFromWidget
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Ix)

newtype GtkBlastVersion
    = GtkBlastVersion {fromGtkBlastVersion :: Version}
  deriving (Eq, Ord)

instance FromJSON CaptchaMode where
    parseJSON = jsonReadInstance

instance ToJSON CaptchaMode where
    toJSON = jsonShowInstance

instance FromJSON PastaSet where
    parseJSON = jsonReadInstance

instance ToJSON PastaSet where
    toJSON = jsonShowInstance

instance FromJSON VideoSet where
    parseJSON = jsonReadInstance

instance ToJSON VideoSet where
    toJSON = jsonShowInstance

instance FromJSON GtkBlastVersion where
    parseJSON (String s) = maybe mzero return $
        GtkBlastVersion . fst <$> lastMay (readP_to_S parseVersion $ T.unpack s)
    parseJSON _ = mzero

instance ToJSON GtkBlastVersion where
    toJSON = String . T.pack . showVersion . fromGtkBlastVersion

instance Show GtkBlastVersion where
    show = showVersion . fromGtkBlastVersion
