module GtkBlast.Pasta
    (PastaSet(..)
    ,generatePastaGen
    ,pastaDate
    ,regeneratePastaGen
    ) where
import Import
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Parsing
import "blast-it-with-piss" BlastItWithPiss.Choice
import "blast-it-with-piss" BlastItWithPiss.MonadChoice
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Type_PastaSet
import System.Directory
import Control.Concurrent.STM
import System.IO.UTF8 (readFile)

readPasta :: FilePath -> IO [String]
readPasta f = filter (not . all isSpace) . delimitByLE "\n\n\n\n" <$> readFile f

generateRandomString :: MonadChoice m => (Int, Int) -> (Char, Char) -> m String
generateRandomString lengthBounds charBounds = do
    len <- getRandomR lengthBounds
    take len <$> getRandomRs charBounds

pastaChooser :: [String] -> E ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO ((Bool, Bool), String))
pastaChooser pastas = do
    E{..} <- ask
    e <- (,) <$> get wcheckescapeinv <*> get wcheckescapewrd
    return $ \_ _ _ -> (,) e <$> mchooseFromList pastas

generatePastaGen :: PastaSet -> E ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO ((Bool, Bool), String))
generatePastaGen Mocha = pastaChooser =<< appFile [] readPasta =<<
#ifdef TEST
                                            return "./testkokoko"
#else
                                            getResourceFile "mocha"
#endif
generatePastaGen PastaFile =
    pastaChooser =<< appFile [] readPasta =<< get =<< asks wentrypastafile
generatePastaGen Char = return $ \_ _ _ -> (,) (False, False) <$>
                            generateRandomString (100, 5000) ('a','z')
generatePastaGen Num = return $ \_ _ _ -> (,) (False , False)<$>
                            generateRandomString (100, 5000) ('0', '9')
generatePastaGen FromThread = return $ \a b c -> (,) (False, False) <$> choosePostToRepost a b c

pastaDate :: PastaSet -> E ModificationTime
pastaDate Mocha = appFile nullTime getModificationTime =<< getResourceFile "mocha"
pastaDate PastaFile =
    appFile nullTime getModificationTime =<< get =<< asks wentrypastafile
pastaDate _ = return timeJustAfterNullTime'ie'forceUpdateJustOnce -- regen just once

regeneratePastaGen :: E ()
regeneratePastaGen = do
    E{..} <- ask
    ps <- io $ readIORef pastaSet
    lastdate <- io $ readIORef pastaMod
    npd <- pastaDate ps
    when (npd > lastdate) $ do
        writeLog "regen pasta"
        io . atomically . writeTVar (tpastagen shS) =<< generatePastaGen ps
        io $ writeIORef pastaMod npd
