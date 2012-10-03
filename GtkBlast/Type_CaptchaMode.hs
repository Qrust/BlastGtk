module GtkBlast.Type_CaptchaMode
    (CaptchaMode(..)
    ) where

data CaptchaMode = Gui
                 | Antigate
    deriving (Eq, Show, Ord, Read, Enum, Bounded)
