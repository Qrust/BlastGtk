module GtkBlast.Image
    (imageEnvPart

    ,emptyImageGen
    ) where
import Import hiding (on)

import GtkBlast.MuVar
import GtkBlast.Log
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.EnvPart
import GtkBlast.GtkUtils

import BlastItWithPiss
import BlastItWithPiss.ImageGen
import BlastItWithPiss.Image

import System.Directory

import Control.Concurrent.STM

import Graphics.UI.Gtk hiding (get, set, Image)

changeFolder :: Entry -> E ()
changeFolder wentryimagefolder = do
    E{ shS } <- ask
    ni <- get wentryimagefolder
    exists <- io $ doesDirectoryExist ni
    if exists
      then do
        writeLog $ "Image folder changed \"" ++ fromString ni ++ "\""
        let !gen = imageGen ni
        io $ atomically $ writeTVar (timagegen shS) gen
      else do
        tempError 5 $ "Папка с пикчами не существует \"" ++ fromString ni ++ "\""

emptyImageGen :: TempGenType Image
emptyImageGen = mkIgnoreGen $ builtinImageGen

imageGen :: FilePath -> TempGenType Image
imageGen imagefolder = mkIgnoreGen $ do
    fromMaybeM builtinImageGen =<< folderImageGen imagefolder

imageEnvPart :: Builder -> EnvPart
imageEnvPart b = EP
    (\e c -> do
        wentryimagefolder <- setir (coImageFolder c) =<< builderGetObject b castToEntry "entryimagefolder"
        wbuttonimagefolder <- builderGetObject b castToButton "buttonimagefolder"

        onFileChooserEntryButton True wbuttonimagefolder wentryimagefolder
            (runE e . tempError 3)
            (runE e $ changeFolder wentryimagefolder)

        postGUIAsync $ runE e $ changeFolder wentryimagefolder

        return wentryimagefolder)
    (\weif c -> do
        cif <- get weif
        return $ c {coImageFolder=cif})
    (const id)
