{-# LANGUAGE NoImplicitPrelude #-}
module GtkBlast.Environment
    (WipeUnit(..)
    ,BoardUnit(..)
    ,Env(..)
    ,E
    ,runE
    ,module Control.Monad.Trans.Reader
    ) where
import Import
import GtkBlast.Directory
import GtkBlast.Type_PastaSet
import GtkBlast.Type_CaptchaMode
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Blast
import "blast-it-with-piss" BlastItWithPiss.Board
import Graphics.UI.Gtk
import GHC.Conc
import Control.Concurrent.STM
import qualified Data.Map as M
import Control.Monad.Trans.Reader

data WipeUnit = WipeUnit {wuProxy :: !BlastProxy
                         ,wuThreadId :: !ThreadId
                         ,wuBanned :: !(IORef Bool)
                         }
    deriving (Eq)

data BoardUnit = BoardUnit {buBoard :: !Board
                           ,buWidget :: !CheckButton
                           ,buWipeUnits :: !(IORef [WipeUnit])
                           -- TODO right now we don't support configuring per-board
                           --      wipe preferences
                           --,buMuSettings :: MuSettings
                           }

data Env = E
    {
     messageLocks :: IORef Int
    ,wipeStarted :: IORef Bool
    ,postCount :: IORef Int
    ,activeCount :: IORef Int
    ,bannedCount :: IORef Int
    ,pastaSet :: IORef PastaSet
    ,pastaMod :: IORef ModificationTime
    ,imagesLast :: IORef [String]
    ,proxies :: IORef (M.Map BlastProxy ProxySettings)
    ,httpproxyMod :: IORef ModificationTime
    ,httpproxyLast :: IORef [BlastProxy]
    ,socksproxyMod :: IORef ModificationTime
    ,socksproxyLast :: IORef [BlastProxy]
    ,captchaMode :: IORef CaptchaMode
    ,pendingAntigateCaptchas :: IORef [(ThreadId, (OriginStamp, Message))]
    ,antigateLogQueue :: TQueue (Either String String)
    ,pendingGuiCaptchas :: IORef [(OriginStamp, Message)]
    ,boardUnits :: [BoardUnit]

    ,tqOut :: TQueue OutMessage
    ,shS :: ShSettings

    ,window :: Window
    ,wbuf :: TextBuffer
    ,wlabelmessage :: Label
    ,wvboxcaptcha :: VBox
    ,wimagecaptcha :: Image
    ,wentrycaptcha :: Entry
    ,wbuttoncaptchaok :: Button
    ,wprogressalignment :: Alignment
    ,wprogresswipe :: ProgressBar
    ,wentryimagefolder :: Entry
    ,wcheckimages :: CheckButton
    ,wcheckannoy :: CheckButton
    ,wcheckhideonsubmit :: CheckButton
    ,wcheckannoyerrors :: CheckButton
    ,wchecktray :: CheckButton
    ,wcheckhttpproxy :: CheckButton
    ,wentryhttpproxyfile :: Entry
    ,wchecksocksproxy :: CheckButton
    ,wentrysocksproxyfile :: Entry
    ,wchecknoproxy :: CheckButton
    ,wentryantigatekey :: Entry
    ,wentrypastafile :: Entry
    }

type E = ReaderT Env IO

-- This is to check that every field is initialized before we start.
instance NFData Env where
    rnf E{..} =
         messageLocks
        `seq` wipeStarted
        `seq` postCount
        `seq` activeCount
        `seq` bannedCount
        `seq` pastaSet
        `seq` pastaMod
        `seq` imagesLast
        `seq` proxies
        `seq` httpproxyMod
        `seq` httpproxyLast
        `seq` socksproxyMod
        `seq` socksproxyLast
        `seq` captchaMode
        `seq` pendingAntigateCaptchas
        `seq` antigateLogQueue
        `seq` pendingGuiCaptchas
        `seq` boardUnits
    
        `seq` tqOut
        `seq` shS
    
        `seq` window
        `seq` wbuf
        `seq` wlabelmessage
        `seq` wvboxcaptcha
        `seq` wimagecaptcha
        `seq` wentrycaptcha
        `seq` wbuttoncaptchaok
        `seq` wprogressalignment
        `seq` wprogresswipe
        `seq` wentryimagefolder
        `seq` wcheckimages
        `seq` wcheckannoy
        `seq` wcheckhideonsubmit
        `seq` wcheckannoyerrors
        `seq` wchecktray
        `seq` wcheckhttpproxy
        `seq` wentryhttpproxyfile
        `seq` wchecksocksproxy
        `seq` wentrysocksproxyfile
        `seq` wchecknoproxy
        `seq` wentryantigatekey
        `seq` wentrypastafile
        `seq` ()

runE :: Env -> E a -> IO a
runE e m = runReaderT m e
