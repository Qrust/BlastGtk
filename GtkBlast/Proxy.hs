module GtkBlast.Proxy
    (regenerateProxies
    ) where
import Import
import GtkBlast.IO
import GtkBlast.Directory
import GtkBlast.MuVar
import GtkBlast.Log
import GtkBlast.Environment
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Blast
import qualified Data.Map as M
import Graphics.UI.Gtk hiding (get, set)
import System.Directory
import System.IO.UTF8 (readFile)

regenerateProxies :: E ()
regenerateProxies = do
    let getProxyMap :: Bool -> CheckButton -> Entry -> IORef ModificationTime -> IORef [BlastProxy] -> E [BlastProxy]
        getProxyMap isSocks wcheckproxy wentryproxyfile proxymod proxylast = do
            ifM (get wcheckproxy)
                (do pf <- get wentryproxyfile
                    d <- get proxymod
                    nd <- appFile nullTime getModificationTime pf
                    if (nd > d)
                        then do
                            writeLog $ "regen " ++ if isSocks then "socks" else "http" ++ " proxy"
                            set proxymod nd
                            nps <- catMaybes . map (readBlastProxy isSocks) . lines <$>
                                    appFile [] readFile pf
                            set proxylast nps
                            return nps
                        else get proxylast)
                (do set proxylast []
                    return [])
    let robustEnterpriseQualityBestPracticesSolution x a = do
            y <- M.fromList <$> forM a (\p -> (,) p <$> io defPrS)
            return $ M.intersection x y `M.union` y
    E{..} <- ask
    nnp <- ifM (get wchecknoproxy) (return [NoProxy]) (return [])
    nhps <- getProxyMap False wcheckhttpproxy wentryhttpproxyfile httpproxyMod httpproxyLast
    nsps <- getProxyMap True wchecksocksproxy wentrysocksproxyfile socksproxyMod socksproxyLast
    modM proxies
        (`robustEnterpriseQualityBestPracticesSolution` (nnp ++ nhps ++ nsps))
