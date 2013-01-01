module BlastItWithPiss.Origin
    (OriginStamp(..)
    ,OriginInfo(..)
    ,renderCompactStamp) where
import Import
import BlastItWithPiss.PostDestination
import BlastItWithPiss.Choice
import BlastItWithPiss.Proxy
import BlastItWithPiss.Board
import qualified Text.Show

data OriginStamp
    = OriginStamp
        {oTime :: !ZonedTime
        ,oProxy :: !Proxy
        ,oBoard :: !Board
        ,oMode :: !Mode
        ,oThread :: !PostDest
        }

instance NFData OriginStamp where
    rnf (OriginStamp t p b m th) = rnf (t,p,b,m,th)

renderCompactStamp :: OriginStamp -> String
renderCompactStamp (OriginStamp _ proxy board _ _) =
    renderBoard board ++ " {" ++ show proxy ++ "}"

instance Show OriginStamp where
    show (OriginStamp time proxy board mode thread) =
        "(" ++ show time ++ ") " ++ "{" ++ show proxy ++ "} " ++ renderBoard board ++
        " " ++ show mode ++ " [| " ++
        ssachThread board thread ++ " |]"

data OriginInfo
    = OriginInfo
        {gmode :: !Mode
        ,gthread :: !PostDest
        }

instance Default OriginInfo where
    def = OriginInfo CreateNew NewThread
