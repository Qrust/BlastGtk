module GtkBlast.Environment
    (Env(..)

    ,E
    ,runE
    ,module Control.Monad.Trans.Reader
    ) where
import Import

import GtkBlast.Directory
import GtkBlast.Types
import GtkBlast.Worker (BoardUnit)

import BlastItWithPiss

import Graphics.UI.Gtk

import GHC.Conc

import Control.Concurrent.Thread.Group (ThreadGroup)

import qualified Data.Map as M

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

data Env = E
    {
     boardUnits :: [BoardUnit]
    ,threadGroup :: ThreadGroup
    ,proxies :: IORef (M.Map BlastProxy ProxySettings)

    ,wipeStarted :: TVar Bool
    ,shS :: ShSettings -- tstartsignal is there

    ,captchaCache :: CaptchaCache Blast (ResourceT IO)
    ,captchaKeysStore :: CaptchaKeysStore Blast (ResourceT IO)
    ,accessPresolverState :: IORef (Maybe (STM PresolverState))

    ,connection :: Manager

    ,messageLocks :: IORef Int

    -- | postCount for whole wipe session
    ,postCount :: IORef Int
    ,wipeStats :: IORef (Int, Int, Int)
    -- | Either (num posts at current wipe start)
    --          (first post timestamp, first post's num in current wipe)
    ,firstPostStats :: IORef (Either Int (UTCTime, Int))
    ,captchasSolved :: IORef Int

    ,pastaSet :: IORef PastaSet
    ,pastaText :: IORef Text

    ,videoSet :: IORef VideoSet
    ,videoText :: IORef Text
    ,wentryvideofile :: Entry

    ,httpproxyMod :: IORef ModificationTime
    ,httpproxyLast :: IORef [BlastProxy]
    ,socksproxyMod :: IORef ModificationTime
    ,socksproxyLast :: IORef [BlastProxy]

    ,captchaMode :: IORef CaptchaMode

    ,pendingAntigateCaptchas :: IORef [(ThreadId, (CaptchaOrigin, CaptchaRequest))]

    ,pendingGuiCaptchas :: IORef [(CaptchaOrigin, CaptchaRequest)]

    -- Widgets
    ,window :: Window

    ,wbuf :: TextBuffer
    ,wcheckannoy :: CheckButton
    ,wcheckannoyerrors :: CheckButton
    ,wspinmaxlines :: SpinButton

    -- added in 1.2
    ,wcheckpresolvecaptcha :: CheckButton

    ,wlabelstats :: Label
    ,wlabelmessage :: Label

    ,wvboxcaptcha :: VBox
    ,wimagecaptcha :: Image
    ,wentrycaptcha :: Entry
    ,wbuttoncaptchaok :: Button
    ,wcheckhideonsubmit :: CheckButton

    ,wprogressalignment :: Alignment
    ,wbuttonwipe :: Button
    ,wprogresswipe :: ProgressBar

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
    }

type E = ReaderT Env IO

-- That is to check that every field is initialized before we start.
instance NFData Env where
    rnf E{..} =
              messageLocks
        `seq` boardUnits
        `seq` wipeStarted
        `seq` postCount
        `seq` wipeStats
        `seq` firstPostStats

        `seq` captchaCache
        `seq` captchaKeysStore
        `seq` accessPresolverState

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

        `seq` pendingGuiCaptchas

        `seq` shS

        `seq` threadGroup

        `seq` connection

        `seq` window

        `seq` wlabelstats
        `seq` wlabelmessage

        `seq` wprogressalignment
        `seq` wbuttonwipe
        `seq` wprogresswipe

        `seq` wvboxcaptcha
        `seq` wimagecaptcha
        `seq` wentrycaptcha
        `seq` wbuttoncaptchaok
        `seq` wcheckhideonsubmit

        `seq` wbuf
        `seq` wcheckannoy
        `seq` wcheckannoyerrors

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

        `seq` wcheckpresolvecaptcha

        `seq` ()

runE :: Env -> E a -> IO a
runE e m = runReaderT m e
