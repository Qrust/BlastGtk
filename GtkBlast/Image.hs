module GtkBlast.Image
    (regenerateImages
    ) where
import Import
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Log
import GtkBlast.Environment
import "blast-it-with-piss" BlastItWithPiss
import System.FilePath
import System.Directory
import Control.Concurrent.STM

filterImages :: [FilePath] -> [FilePath]
filterImages = filter ((`elem` [".jpg",".jpeg",".gif",".png"]) . takeExtension)

regenerateImages :: E ()
regenerateImages = do
    E{..} <- ask
    ni <- get wentryimagefolder
    images <- filterImages <$> fromIOEM
                (do whenM (get wcheckimages) $ tempError 3 $ "Невозможно прочитать изображения из папки " ++ ni
                    return [])
                (io $ map (ni </>) <$> getDirectoryContents ni)
    li <- get imagesLast
    when (images /= li) $ do
        writeLog "regen images"
        io $ atomically $ writeTVar (timages shS) images
        set imagesLast images
