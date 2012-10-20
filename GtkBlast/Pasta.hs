module GtkBlast.Pasta
    (PastaSet(..)
    ,generatePastaGen
    ,pastaDate
    ,regeneratePastaGen
    ,pastaEnvPart
    ) where
import Import hiding (on)
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Parsing
import "blast-it-with-piss" BlastItWithPiss.Choice
import "blast-it-with-piss" BlastItWithPiss.MonadChoice
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.EnvPart
import GtkBlast.Log
import GtkBlast.Type_PastaSet
import GtkBlast.GtkUtils
import Control.Monad.Fix
import System.Directory
import Control.Concurrent.STM
import System.IO.UTF8 (readFile)
import Graphics.UI.Gtk hiding (get,set)

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
generatePastaGen PastaFile =
    pastaChooser =<< appFile [] readPasta =<< get =<< asks wentrypastafile
generatePastaGen Char = return $ \_ _ _ -> (,) (False, False) <$>
                            generateRandomString (100, 5000) ('a','z')
generatePastaGen Num = return $ \_ _ _ -> (,) (False , False)<$>
                            generateRandomString (100, 5000) ('0', '9')
generatePastaGen FromThread = return $ \a b c -> (,) (False, False) <$> choosePostToRepost a b c

pastaDate :: PastaSet -> E ModificationTime
pastaDate PastaFile =
    appFile nullTime getModificationTime =<< get =<< asks wentrypastafile
pastaDate _ = return timeJustAfterNullTime'ie'forceUpdateJustOnce -- HACK regen just once

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

pastaEnvPart :: Builder -> EnvPart
pastaEnvPart b = EP
    (\e c -> do
        wradiofromthread <- builderGetObject b castToRadioButton "radio-fromthread"
        wradionum <- builderGetObject b castToRadioButton "radio-num"
        wradiochar <- builderGetObject b castToRadioButton "radio-char"
        wradiopastafile <- builderGetObject b castToRadioButton "radio-pastafile"
    
        let pastaradio =
                [(FromThread, wradiofromthread)
                ,(Num, wradionum)
                ,(Char, wradiochar)
                ,(PastaFile, wradiopastafile)
                ]
    
        fromMaybe (return ()) $ (`findMap` pastaradio) $ \(p, w) ->
            if p == coPastaSet c
                then Just $ toggleButtonSetActive w True
                else Nothing

        pastaMod <- newIORef nullTime
        pastaSet <- newIORef $ coPastaSet c

        forM_ pastaradio $ \(p, w) -> do
            void $ on w toggled $
                whenM (toggleButtonGetActive w) $ do
                    writeIORef pastaSet p
                    writeIORef pastaMod nullTime -- force update
                    runE e $ regeneratePastaGen

        wentrypastafile <- (rec coPastaFile $ builderGetObject b castToEntry "entrypastafile") e c
        wbuttonpastafile <- builderGetObject b castToButton "buttonpastafile"

        onFileChooserEntryButton False wbuttonpastafile wentrypastafile (runE e . writeLog) $ do
            whenM ((==PastaFile) <$> readIORef pastaSet) $ do
                writeIORef pastaMod nullTime -- force update
                runE e $ regeneratePastaGen

        wcheckescapeinv <- (rec coEscapeInv $ builderGetObject b castToCheckButton "checkescapeinv") e c
        wcheckescapewrd <- (rec coEscapeWrd $ builderGetObject b castToCheckButton "checkescapewrd") e c

        let bolall w1 w2 = do
                (x1,x2) <- (,) <$> getIO w1 <*> getIO w2
                return $ if x1==x2 then Just x1 else Nothing

        wcheckescapeall <- builderGetObject b castToCheckButton "checkescapeall"

        wceas <- mfix $ \ ~wceas -> do
            on wcheckescapeall buttonActivated $ do
                x <- bolall wcheckescapeinv wcheckescapewrd
                case x of
                    Just a -> do
                        toggleButtonSetInconsistent wcheckescapeall False
                        signalBlock wceas
                        setIO wcheckescapeinv $ not a
                        setIO wcheckescapewrd $ not a
                        setIO wcheckescapeall $ not a
                        signalUnblock wceas
                    Nothing -> do
                        toggleButtonSetInconsistent wcheckescapeall True

        let updAll = do
                signalBlock wceas
                maybe (toggleButtonSetInconsistent wcheckescapeall True)
                      (\a -> do
                        toggleButtonSetInconsistent wcheckescapeall False
                        setIO wcheckescapeall a) =<<
                    bolall wcheckescapeinv wcheckescapewrd
                signalUnblock wceas

        void $ on wcheckescapeinv buttonActivated $ do
            updAll
            writeIORef pastaMod nullTime -- force update
            runE e $ regeneratePastaGen

        void $ on wcheckescapewrd buttonActivated $ do
            updAll
            writeIORef pastaMod nullTime -- force update
            runE e $ regeneratePastaGen

        updAll

        return (pastaSet, pastaMod, wentrypastafile, wcheckescapeinv, wcheckescapewrd)
        )
    (\(v1,_,v2,v3,v4) c -> do
        ps <- get v1
        pf <- get v2
        ei <- get v3
        ew <- get v4
        return c{coPastaSet=ps, coPastaFile=pf, coEscapeInv=ei, coEscapeWrd=ew})
    (\(ps,pm,wepf,wcei,wcew) e -> e{pastaSet=ps
                                   ,pastaMod=pm
                                   ,wentrypastafile=wepf
                                   ,wcheckescapeinv=wcei
                                   ,wcheckescapewrd=wcew
                                   })
