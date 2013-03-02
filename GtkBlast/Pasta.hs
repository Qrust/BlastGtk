module GtkBlast.Pasta
    (PastaSet(..)
    ,generatePastaGen
    ,pastaDate
    ,regeneratePastaGen
    ,pastaEnvPart
    ) where
import Import hiding (on)
import BlastItWithPiss
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.EnvPart
import GtkBlast.Log
import GtkBlast.Type_PastaSet
import GtkBlast.GtkUtils
import System.Random.Shuffle
import System.Directory
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.ByteString as B
import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as G (get)

readPasta :: FilePath -> IO [String]
readPasta f =
    filter (not . all isSpace) . delimitByLE "\n\n\n\n" .
        T.unpack . decodeUtf8 <$> B.readFile f

generatePastaGen :: PastaSet -> E ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO TempBlastCaptchaChannel)
generatePastaGen PastaFile = do
    E{..} <- ask
    pastas <- appFile [] readPasta =<< get wentrypastafile
    toQuote <- get wcheckrandomquote
    let rquoter = if toQuote then genPastaRandomQuote 100 0 else \_ _ _ -> return
    ei <- get wcheckescapeinv
    ew <- get wcheckescapewrd
    let x a b c = fmap (TBCC (null pastas) ei ew) $
                    rquoter a b c =<< mchooseFromList pastas
    return x
generatePastaGen Symbol = do
    E{..} <- ask
    toQuote <- get wcheckrandomquote
    let rquoter = if toQuote then genPastaRandomQuote 100 0 else \_ _ _ -> return
    return $ \a b c -> fmap (TBCC False False False) $
        rquoter a b c =<< generateSymbolString 5000
generatePastaGen FromThread = do
    E{..} <- ask

    toShuf <- get wcheckshufflereposts
    let shuf = if toShuf then shuf' else return

    quote <- get wcheckrandomquote

    let x a b c = fmap (TBCC False False False) $ shuf =<< genPastaFromReposts quote a b c
    return x
  where
    shuf' = fmap unwords . shuffleM . words
generatePastaGen NoPasta = do
    E{..} <- ask
    toQuote <- get wcheckrandomquote
    let rquoter = if toQuote then genPastaRandomQuote 100 0 else \_ _ _ -> return
    return $ \a b c -> TBCC True False False <$> rquoter a b c ""

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
        pastaMod <- newIORef nullTime
        pastaSet <- newIORef $ coPastaSet c

        wcheckescapeinv <- (rec coEscapeInv $ builderGetObject b castToCheckButton "checkescapeinv") e c
        wcheckescapewrd <- (rec coEscapeWrd $ builderGetObject b castToCheckButton "checkescapewrd") e c
        wcheckshufflereposts <- (rec coShuffleReposts $ builderGetObject b castToCheckButton "checkshufflereposts") e c
        wcheckrandomquote <- (rec coRandomQuote $ builderGetObject b castToCheckButton "checkrandomquote") e c

        let bolall w1 w2 = do
                (x1,x2) <- (,) <$> getIO w1 <*> getIO w2
                return $ if x1==x2 then Just x1 else Nothing

        wcheckescapeall <- builderGetObject b castToCheckButton "checkescapeall"

        ignore <- newIORef False

        void $ on wcheckescapeall buttonActivated $ unlessM (get ignore) $ do
            x <- bolall wcheckescapeinv wcheckescapewrd
            case x of
                Just a -> do
                    toggleButtonSetInconsistent wcheckescapeall False
                    set ignore True
                    setIO wcheckescapeinv $ not a
                    setIO wcheckescapewrd $ not a
                    setIO wcheckescapeall $ not a
                    set ignore False
                Nothing -> do
                    toggleButtonSetInconsistent wcheckescapeall True

        wradiofromthread <- builderGetObject b castToRadioButton "radio-fromthread"
        wradiosym <- builderGetObject b castToRadioButton "radio-symbol"
        wradiopastafile <- builderGetObject b castToRadioButton "radio-pastafile"
        wradionopasta <- builderGetObject b castToRadioButton "radio-nopasta"

        let pastaradio =
                [(FromThread, wradiofromthread)
                ,(Symbol, wradiosym)
                ,(PastaFile, wradiopastafile)
                ,(NoPasta, wradionopasta)
                ]

        pwcei <- newIORef =<< get wcheckescapeinv
        pwcew <- newIORef =<< get wcheckescapewrd
        let pastafileassoc = [(wcheckescapeinv, pwcei), (wcheckescapewrd, pwcew)]
        let pastafilewidgets = [wcheckescapeall, wcheckescapeinv, wcheckescapewrd]

        pwcsr <- newIORef =<< get wcheckshufflereposts
        let fromthreadassoc = [(wcheckshufflereposts, pwcsr)]
        let fromthreadwidgets = [wcheckshufflereposts]

        let setSensitive t assoc senswidgets = do
                if t
                    then
                        mapM_ (\(w,p) -> toggleButtonSetActive w =<< get p) assoc
                    else do
                        forM_ assoc $ \(w,p) -> do
                            whenM (G.get w widgetSensitive) $
                                set p =<< get w
                            toggleButtonSetActive w False
                mapM_ (`widgetSetSensitive` t) senswidgets

        void $ flip anyM pastaradio $ \(p, w) ->
            if (p == coPastaSet c)
                then True <$ do
                    toggleButtonSetActive w True
                    setSensitive (p==PastaFile) pastafileassoc pastafilewidgets
                    setSensitive (p==FromThread) fromthreadassoc fromthreadwidgets
                else return False

        forM_ pastaradio $ \(p, w) -> do
            void $ on w toggled $
                whenM (toggleButtonGetActive w) $ do
                    writeIORef pastaSet p
                    writeIORef pastaMod nullTime -- force update
                    runE e $ regeneratePastaGen
                    setSensitive (p==PastaFile) pastafileassoc pastafilewidgets
                    setSensitive (p==FromThread) fromthreadassoc fromthreadwidgets

        wentrypastafile <- (rec coPastaFile $ builderGetObject b castToEntry "entrypastafile") e c
        wbuttonpastafile <- builderGetObject b castToButton "buttonpastafile"

        onFileChooserEntryButton False wbuttonpastafile wentrypastafile (runE e . writeLog) $ do
            whenM ((==PastaFile) <$> readIORef pastaSet) $ do
                writeIORef pastaMod nullTime -- force update
                runE e $ regeneratePastaGen

        let updAll = unlessM (get ignore) $ do
                set ignore True
                maybe (toggleButtonSetInconsistent wcheckescapeall True)
                      (\a -> do
                        toggleButtonSetInconsistent wcheckescapeall False
                        setIO wcheckescapeall a) =<<
                    bolall wcheckescapeinv wcheckescapewrd
                set ignore False

        void $ on wcheckescapeinv buttonActivated $ do
            updAll
            writeIORef pastaMod nullTime -- force update
            runE e $ regeneratePastaGen

        void $ on wcheckescapewrd buttonActivated $ do
            updAll
            writeIORef pastaMod nullTime -- force update
            runE e $ regeneratePastaGen

        void $ on wcheckshufflereposts buttonActivated $ do
            writeIORef pastaMod nullTime -- force update
            runE e $ regeneratePastaGen

        void $ on wcheckrandomquote buttonActivated $ do
            writeIORef pastaMod nullTime -- force update
            runE e $ regeneratePastaGen

        updAll

        return (pastaSet, pastaMod, wentrypastafile, wcheckescapeinv, wcheckescapewrd, pwcei, pwcew, wcheckshufflereposts, pwcsr, wcheckrandomquote)
        )
    (\(ps',_,wepf,wei,wew,pwei,pwew,wsr,pwsr,wcrq) c -> do
        ps <- get ps'
        pf <- get wepf
        ei <- ifM (G.get wei widgetSensitive) (get wei) (get pwei)
        ew <- ifM (G.get wew widgetSensitive) (get wew) (get pwew)
        sr <- ifM (G.get wsr widgetSensitive) (get wsr) (get pwsr)
        rq <- get wcrq
        return c{coPastaSet=ps
                ,coPastaFile=pf
                ,coEscapeInv=ei
                ,coEscapeWrd=ew
                ,coShuffleReposts=sr
                ,coRandomQuote=rq})
    (\(ps,pm,wepf,wcei,wcew,_,_,wcsr,_,wcrq) e ->
        e{pastaSet=ps
         ,pastaMod=pm
         ,wentrypastafile=wepf
         ,wcheckescapeinv=wcei
         ,wcheckescapewrd=wcew
         ,wcheckshufflereposts=wcsr
         ,wcheckrandomquote=wcrq
         })
