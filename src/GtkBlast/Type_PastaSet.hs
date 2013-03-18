module GtkBlast.Type_PastaSet
    (PastaSet(..)
    ) where
import Prelude

data PastaSet = PastaFile
              | Symbol
              | FromThread
              | NoPasta
    deriving (Eq, Show, Ord, Read, Enum, Bounded)
