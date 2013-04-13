module GtkBlast.Image
    (imageGen
    ,emptyImageGen
    ,imageEnvPart
    ) where
import Import hiding (on)

import GtkBlast.MuVar
import GtkBlast.Log
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.EnvPart
import GtkBlast.GtkUtils
import GtkBlast.Directory

import BlastItWithPiss
import BlastItWithPiss.ImageGen
import BlastItWithPiss.Image
import BlastItWithPiss.MonadChoice

import System.Directory

import Control.Concurrent.STM

import Graphics.UI.Gtk hiding (get, set, Image)

changeFolder :: IORef (Maybe CloseWatcher) -> E ()
changeFolder mcw = do
    maybe (return ()) closeWatcher =<< get mcw
    set mcw Nothing
    e@E{..} <- ask
    ni <- get wentryimagefolder
    exists <- io $ doesDirectoryExist ni
    if exists
      then do
        agitka <- get wcheckagitka
        writeLog $
            "Image folder changed \""
            ++ toText ni ++ "\", agitka: " ++ show agitka
        cw <- postAsyncWhenPathModified ni $
            runE e $ changeFolder mcw
        set mcw $ Just cw
        let !gen = imageGen ni agitka
        io $ atomically $ writeTVar (timagegen shS) gen
      else do
        tempError 5 $ "Папка с пикчами не существует \"" ++ toText ni ++ "\""

emptyImageGen :: TempGenType Image
emptyImageGen = mkIgnoreGen $ builtinImageGen

imageGen :: FilePath -> Bool -> TempGenType Image
imageGen imagefolder agitka = mkIgnoreGen $ do
    images <- getDirectoryPics imagefolder

    let agitkafile = bundledFile "resources/agitka.png"
    thereIsAgitkaFile <- doesFileExist agitkafile

    useAgitka <-
      if agitka && thereIsAgitkaFile
        then
          if null images
            then
              return True
            else
              fromList [(True, 15), (False, 85)]
        else
          return False

    if useAgitka
      then
        readImageWithoutJunk agitkafile
      else
        fromMaybeM builtinImageGen =<<
          if null images
            then return Nothing
            else do
              fromIOException (return Nothing) $ fmap Just $
                readImageWithoutJunk =<< chooseFromList images

imageEnvPart :: Builder -> EnvPart
imageEnvPart b = EP
    (\e c -> do
        wentryimagefolder <- setir (coImageFolder c) =<< builderGetObject b castToEntry "entryimagefolder"
        wbuttonimagefolder <- builderGetObject b castToButton "buttonimagefolder"
        wcheckagitka <- setir (coPostAgitka c) =<< builderGetObject b castToCheckButton "checkagitka"

        mclose <- newIORef Nothing

        onFileChooserEntryButton True wbuttonimagefolder wentryimagefolder
            (runE e . tempError 3)
            (runE e $ changeFolder mclose)

        void $ on wcheckagitka buttonActivated $
            runE e $ changeFolder mclose

        void $ postAsyncWhenPathModified (coImageFolder c) $
            runE e $ changeFolder mclose

        postGUIAsync $ runE e $ changeFolder mclose

        return (wentryimagefolder, wcheckagitka))
    (\(weif,wca) c -> do
        cif <- get weif
        cpa <- get wca
        return $ c {coImageFolder=cif
                   ,coPostAgitka=cpa})
    (\(weif,wca) e ->
        e{wentryimagefolder=weif
         ,wcheckagitka=wca})
