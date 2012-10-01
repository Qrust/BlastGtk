{-# LANGUAGE NoImplicitPrelude #-}
module GtkBlast.Pasta
    (PastaSet(..)
    ,generatePasta
    ,pastaDate
    ,regeneratePasta
    ) where
import Import
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.MonadChoice
import GtkBlast.IO
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Log
import System.Directory
import Control.Concurrent.STM

data PastaSet = Mocha
              | Kakashki
              | Num
              | Char
              | FromThread
    deriving (Eq, Show, Ord, Read, Enum, Bounded)

readPasta :: FilePath -> IO [String]
readPasta f = filter (not . all isSpace) . delimitByLE "\n\n\n\n" <$> readFile f

generateRandomString :: MonadChoice m => (Int, Int) -> (Char, Char) -> m String
generateRandomString lengthBounds charBounds = do
    len <- getRandomR lengthBounds
    take len <$> getRandomRs charBounds

generateRandomStrings :: MonadChoice m => (Int, Int) -> (Int, Int) -> (Char, Char) -> m [String]
generateRandomStrings lengthBounds a b = do
    len <- getRandomR lengthBounds
    replicateM len $ generateRandomString a b

generatePasta :: PastaSet -> E [String]
generatePasta Mocha = appFile [] readPasta =<<
#ifdef TEST
                                            return "./testkokoko"
#else
                                            getResourceFile "mocha"
#endif
generatePasta Kakashki = appFile [] readPasta =<< getResourceFile "sadism"
generatePasta Char = generateRandomStrings (1, 30) (100, 5000) ('a','z')
generatePasta Num = generateRandomStrings (1, 30) (100, 5000) ('0', '9')
generatePasta FromThread = error "FIXME NOT IMPLEMENTED"

pastaDate :: PastaSet -> E ModificationTime
pastaDate Mocha = appFile nullTime getModificationTime =<< getResourceFile "mocha"
pastaDate Kakashki = appFile nullTime getModificationTime =<< getResourceFile "sadism"
pastaDate _ = timeRightNow

regeneratePasta :: E ()
regeneratePasta = do
    E{..} <- ask
    ps <- io $ readIORef pastaSet
    lastdate <- io $ readIORef pastaMod
    npd <- pastaDate ps
    when (npd > lastdate) $ do
        writeLog "regen pasta"
        io . atomically . writeTVar (tpastas shS) =<< generatePasta ps
        io $ writeIORef pastaMod npd
