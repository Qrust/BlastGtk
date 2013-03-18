module GtkBlast.Image
    (regenerateImages
    ,imageGen
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

-- FIXME ? Bind it to signal, why the fuck should it run all the time?
regenerateImages :: E ()
regenerateImages = do
    E{..} <- ask
    ni <- get wentryimagefolder
    li <- get imagefolderLast
    when (ni /= li) $ do
        writeLog "Regen imageGen"
        agitka <- get wcheckagitka
        let !gen = imageGen ni agitka
        io $ atomically $ writeTVar (timagegen shS) gen
        set imagefolderLast ni

imageGen :: FilePath -> Bool -> IO Image
imageGen imagefolder agitka = do
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
        fromMaybeM builtinImageGen $
          if null images
            then return Nothing
            else do
              fromIOException (return Nothing) $ fmap Just $
                readImageWithoutJunk =<< chooseFromList images

imageEnvPart :: Builder -> EnvPart
imageEnvPart b = EP
    (\e c -> do
        wentryimagefolder <- (rec coImageFolder $ builderGetObject b castToEntry "entryimagefolder") e c
        wbuttonimagefolder <- builderGetObject b castToButton "buttonimagefolder"
        
        onFileChooserEntryButton True wbuttonimagefolder wentryimagefolder (runE e . writeLog) (return ())

        wcheckagitka <- (rec coPostAgitka $ builderGetObject b castToCheckButton "checkagitka") e c

        void $ on wcheckagitka buttonActivated $
            runE e $ do
                set (imagefolderLast e) [] -- force update
                regenerateImages

        imagefolderLast <- newIORef []

        return (wentryimagefolder, imagefolderLast, wcheckagitka))
    (\(weif,_,wca) c -> do
        cif <- get weif
        cpa <- get wca
        return $ c {coImageFolder=cif
                   ,coPostAgitka=cpa})
    (\(weif,ifl,wca) e -> e{wentryimagefolder=weif, imagefolderLast=ifl, wcheckagitka=wca})
