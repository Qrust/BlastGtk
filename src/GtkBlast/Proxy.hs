module GtkBlast.Proxy
    (regenerateProxies
    ) where
import Import

import GtkBlast.Directory
import GtkBlast.MuVar
import GtkBlast.Log
import GtkBlast.Environment

import BlastItWithPiss
import BlastItWithPiss.Blast
import BlastItWithPiss.ProxyReader

import qualified Data.Map as M

import Graphics.UI.Gtk hiding (get, set)

import System.Directory

regenerateProxies :: E ()
regenerateProxies = do
    E{..} <- ask
    nnp <- ifM (get wchecknoproxy) (return [NoProxy]) (return [])
    nhps <- getProxyMap False wcheckhttpproxy wentryhttpproxyfile httpproxyMod httpproxyLast
    nsps <- getProxyMap True wchecksocksproxy wentrysocksproxyfile socksproxyMod socksproxyLast
    modM proxies
        (`robustEnterpriseQualityBestPracticesSolution` (nnp ++ nhps ++ nsps))

getProxyMap
    :: Bool -> CheckButton -> Entry -> IORef ModificationTime -> IORef [BlastProxy]
    -> E [BlastProxy]
getProxyMap isSocks wcheckproxy wentryproxyfile proxymod proxylast = do
    enabled <- get wcheckproxy
    pf <- get wentryproxyfile
    if enabled && not (null pf)
      then do
        d <- get proxymod
        nd <- appFile nullTime getModificationTime pf
        if (nd > d)
          then do
            let pType = if isSocks then "socks" else "http" :: Text
            writeLog $ "regen " ++ pType ++ " proxy"
            set proxymod nd
            nps' <- liftIO $ readProxyFile isSocks pf
            nps <- forMaybeM nps' $ either
                (\ip -> Nothing <$ (writeLog $
                    pType
                 ++ " file \"" ++ fromString pf ++ "\": "
                 ++ "Couln't parse as a proxy \""
                 ++ ip ++ "\""))
                (return . Just)
            set proxylast nps
            return nps
          else
            get proxylast
      else do
        set proxylast []
        return []

robustEnterpriseQualityBestPracticesSolution
    :: M.Map BlastProxy ProxySettings -> [BlastProxy]
    -> E (M.Map BlastProxy ProxySettings)
robustEnterpriseQualityBestPracticesSolution x a = do
    y <- M.fromList <$> forM a (\p -> (,) p <$> io defPrS)
    return $ M.intersection x y `M.union` y
