module GtkBlast.Worker where
import Import

import BlastItWithPiss
--import BlastItWithPiss.Blast
--import BlastItWithPiss.Board

import Graphics.UI.Gtk

import GHC.Conc

data WipeUnit = WipeUnit
    {wuProxy    :: !BlastProxy
    ,wuThreadId :: !ThreadId
    }
  deriving Eq

data BoardUnit = BoardUnit
    {buBoard        :: !Board
    ,buWidget       :: !CheckButton
    ,buWipeUnits    :: !(IORef [WipeUnit])
    ,buBanned       :: !(IORef [BlastProxy])
    ,buDead         :: !(IORef [BlastProxy])
    ,buMuSettings   :: !MuSettings
    }
