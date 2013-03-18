module GtkBlast.Type_CaptchaMode
    (CaptchaMode(..)
    ) where
import Prelude

data CaptchaMode = Gui
                 | Antigate
    deriving (Eq, Show, Ord, Read, Enum, Bounded)
