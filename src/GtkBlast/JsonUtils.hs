module GtkBlast.JsonUtils
    (jsonReadInstance
    ,jsonShowInstance
    ) where
import Import

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types

{-# INLINE jsonReadInstance #-}
jsonReadInstance :: Read a => Value -> Parser a
jsonReadInstance = go
  where
    go (String s) = maybe mzero return $ readMay $ T.unpack s
    go _ = mzero

{-# INLINE jsonShowInstance #-}
jsonShowInstance :: Show a => a -> Value
jsonShowInstance = String . T.pack . show
