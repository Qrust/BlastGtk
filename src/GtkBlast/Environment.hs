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
import GtkBlast.Types
import GtkBlast.Worker

import BlastItWithPiss
import BlastItWithPiss.Blast

import Graphics.UI.Gtk

import GHC.Conc
import Control.Concurrent.STM

import Control.Concurrent.Thread.Group (ThreadGroup)

import qualified Data.Map as M

import Control.Monad.Trans.Reader

data Env = E
    {
     boardUnits :: [BoardUnit]
    ,messageLocks :: IORef Int
    ,wipeStarted :: IORef Bool
    ,postCount :: IORef Int
    ,wipeStats :: IORef (Int, Int, Int)

    ,pastaSet :: IORef PastaSet
    ,pastaText :: IORef Text

    ,videoSet :: IORef VideoSet
    ,videoText :: IORef Text
    ,wentryvideofile :: Entry

    ,proxies :: IORef (M.Map BlastProxy ProxySettings)

    ,httpproxyMod :: IORef ModificationTime
    ,httpproxyLast :: IORef [BlastProxy]
    ,socksproxyMod :: IORef ModificationTime
    ,socksproxyLast :: IORef [BlastProxy]

    ,captchaMode :: IORef CaptchaMode
    ,pendingAntigateCaptchas :: IORef [(ThreadId, (OriginStamp, SupplyCaptcha))]
    ,antigateLogQueue :: TQueue (Either Text Text)

    ,pendingGuiCaptchas :: IORef [(OriginStamp, SupplyCaptcha)]
    ,guiReportQueue :: TQueue OriginStamp

    ,shS :: ShSettings

    ,threadGroup :: ThreadGroup

    ,connection :: Manager

    ,window :: Window
    ,wbuf :: TextBuffer
    ,wlabelmessage :: Label
    ,wvboxcaptcha :: VBox
    ,wimagecaptcha :: Image
    ,wentrycaptcha :: Entry
    ,wbuttoncaptchaok :: Button
    ,wprogressalignment :: Alignment
    ,wbuttonwipe :: Button
    ,wprogresswipe :: ProgressBar
    ,wentryimagefolder :: Entry
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
    ,wentryantigatehost :: Entry
    ,wentrypastafile :: Entry
    ,wcheckescapeinv :: CheckButton
    ,wcheckescapewrd :: CheckButton
    ,wcheckshuffle :: CheckButton
    ,wcheckrandomquote :: CheckButton
    ,wspinmaxlines :: SpinButton
    }

type E = ReaderT Env IO

-- This is to check that every field is initialized before we start.
instance NFData Env where
    rnf E{..} =
              messageLocks
        `seq` boardUnits
        `seq` wipeStarted
        `seq` postCount
        `seq` wipeStats

        `seq` pastaSet
        `seq` pastaText

        `seq` videoSet
        `seq` videoText
        `seq` wentryvideofile

        `seq` proxies

        `seq` httpproxyMod
        `seq` httpproxyLast
        `seq` socksproxyMod
        `seq` socksproxyLast
        `seq` captchaMode

        `seq` pendingAntigateCaptchas
        `seq` antigateLogQueue

        `seq` pendingGuiCaptchas
        `seq` guiReportQueue

        `seq` shS

        `seq` threadGroup

        `seq` connection

        `seq` window
        `seq` wbuf
        `seq` wlabelmessage
        `seq` wvboxcaptcha
        `seq` wimagecaptcha
        `seq` wentrycaptcha
        `seq` wbuttoncaptchaok
        `seq` wprogressalignment
        `seq` wbuttonwipe
        `seq` wprogresswipe
        `seq` wentryimagefolder
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
        `seq` wentryantigatehost
        `seq` wentrypastafile
        `seq` wcheckescapeinv
        `seq` wcheckescapewrd
        `seq` wcheckshuffle
        `seq` wspinmaxlines
        `seq` ()

runE :: Env -> E a -> IO a
runE e m = runReaderT m e
