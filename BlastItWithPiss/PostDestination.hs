module BlastItWithPiss.PostDestination
    (PostDest(..)
    ) where
import Import

data PostDest
    = Thread !Int
    | NewThread
  deriving (Eq, Show)

instance NFData PostDest
