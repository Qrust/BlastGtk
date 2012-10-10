module GtkBlast.Type_PastaSet
    (PastaSet(..)
    ) where
import Prelude

data PastaSet = Mocha
              | PastaFile
              | Num
              | Char
              | FromThread
    deriving (Eq, Show, Ord, Read, Enum, Bounded)
