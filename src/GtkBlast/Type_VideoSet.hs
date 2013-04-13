module GtkBlast.Type_VideoSet
    (VideoSet(..)
    ) where
import Prelude

data VideoSet
    = VideoNothing
    | VideoFromFile
    | VideoFromWidget
  deriving (Eq, Show, Ord, Read, Enum, Bounded)
