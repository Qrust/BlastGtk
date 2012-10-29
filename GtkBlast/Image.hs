module GtkBlast.Image
    (regenerateImages
    ,imageGen
    ,imageEnvPart
    ) where
import Import hiding (on)
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Log
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.EnvPart
import GtkBlast.GtkUtils
import GtkBlast.Directory
import BlastItWithPiss
import BlastItWithPiss.Blast
import BlastItWithPiss.Post
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.MonadChoice
import System.FilePath
import System.Directory
import Control.Concurrent.STM
import Graphics.UI.Gtk hiding (get, set, Image)

filterImages :: [FilePath] -> [FilePath]
filterImages = filter ((`elem` [".jpg",".jpeg",".gif",".png"]) . takeExtension)

regenerateImages :: E ()
regenerateImages = do
    E{..} <- ask
    ni <- get wentryimagefolder
    li <- get imagefolderLast
    when (ni /= li) $ do
        writeLog "regen images"
        io . atomically . writeTVar (timagegen shS) . imageGen ni =<< get wcheckagitka
        set imagefolderLast ni

imageGen :: FilePath -> Bool -> Bool -> IO (Bool, Image)
imageGen imagefolder agitka use = do
    images <-
        if use
            then fromIOEM (return []) $ filterImages . map (imagefolder </>) <$> getDirectoryContents imagefolder
            else return []
    agitkafile <- getResourceFile "agitka.png"
    ifM ((agitka &&) <$> doesFileExist agitkafile)
        (do let eq = 100 / (fromIntegral $ length images + 1)
            (,) False <$> (readImageWithoutJunk =<< fromList ((agitkafile, 15) : map (\i -> (i, eq)) images)))
        (if null images
            then (,) True . Image "haruhi.jpg" "image/jpeg" <$> runBlast -- use recaptcha as a fallback
                    (getCaptchaImage =<< getChallengeKey ssachRecaptchaKey)
            else (,) False <$> (readImageWithoutJunk =<< chooseFromList images))

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
