module GtkBlast.Pasta
    (PastaSet(..)

    ,pastaEnvPart

    ,emptyPastaGen
    ) where
import Import hiding (on)

import GtkBlast.MuVar
import GtkBlast.Environment
import GtkBlast.Conf
import GtkBlast.EnvPart
import GtkBlast.Log
import GtkBlast.Types
import GtkBlast.GtkUtils

import BlastItWithPiss
import BlastItWithPiss.PastaGen
import BlastItWithPiss.Parsing
import BlastItWithPiss.MonadChoice

import System.Random.Shuffle

import System.Directory (doesFileExist)

import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as G (get)

import Control.Concurrent.STM

{-# INLINE shuffleWords #-}
shuffleWords :: String -> IO String
shuffleWords = fmap unwords . shuffleM . words

-- | Depending on the environment, generate a function appending a random quote
-- or return text intact
makeRQuoter
    :: E ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> String -> IO String)
makeRQuoter = do
    toQuote <- get =<< asks wcheckrandomquote
    if toQuote
      then return (genPastaRandomQuote 100 0)
      else return (\_ _ _ -> return)

-- | Depending on the environment, generate a function to shuffle words in text
-- or return text intact
makeShuffler :: E (String -> IO String)
makeShuffler = do
    toShuf <- get =<< asks wcheckshuffle
    if toShuf
      then return shuffleWords
      else return return

-- | > 'makeShuffler' '.' 'makeRquoter'
makePostProc
    :: E ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> String -> IO String)
makePostProc = do
    rquoter <- makeRQuoter
    shuffler <- makeShuffler
    return (\a b c s -> shuffler =<< rquoter a b c s)

emptyPastaGen :: TempGenType TempBlastPastaChannel
emptyPastaGen = mkConstGen $ TBPC False False "Обосрался, блядь"

generatePastaGen :: PastaSet -> E (TempGenType TempBlastPastaChannel)
generatePastaGen PastaFile = do
    E{..} <- ask

    !pastafile <- get wentrypastafile
    !pastas <- appFile [] (fmap (fromMaybe []) . readPastaFile) pastafile

    !postproc <- makePostProc

    !ei <- get wcheckescapeinv
    !ew <- get wcheckescapewrd

    return $ mkFullGen $ \a b c -> fmap (TBPC ei ew) $
        postproc a b c =<< fromMaybe "" <$> chooseFromListMaybe pastas
generatePastaGen Symbol = do
    E{..} <- ask

    !postproc <- makePostProc

    return $ mkFullGen $ \a b c -> fmap (TBPC False False) $
        postproc a b c =<< generateSymbolString 5000
generatePastaGen FromThread = do
    E{..} <- ask

    !shuffler <- makeShuffler

    !quote <- get wcheckrandomquote

    return $ mkFullGen $ \a b c -> fmap (TBPC False False) $
        shuffler =<< genPastaFromReposts quote a b c
generatePastaGen NoPasta = do
    E{..} <- ask

    !postproc <- makePostProc

    return $ mkFullGen $ \a b c -> fmap (TBPC False False) $
        postproc a b c ""
generatePastaGen FromWidget = do
    E{..} <- ask

    !pastas <- fromMaybe [] . parsePasta <$> get pastaText

    !postproc <- makePostProc

    !ei <- get wcheckescapeinv
    !ew <- get wcheckescapewrd

    return $ mkFullGen $ \a b c -> fmap (TBPC ei ew) $
        postproc a b c =<< fromMaybe "" <$> chooseFromListMaybe pastas

regeneratePastaGen :: IORef (Maybe CloseWatcher) -> E ()
regeneratePastaGen mcw = do
    e@E{..} <- ask

    newfname <- get wentrypastafile
    ps <- get pastaSet

    maybe (return ()) closeWatcher =<< get mcw
    set mcw Nothing

    writeLog $
        "Pasta changed " ++
            (if ps==PastaFile
              then show ps ++ ":\"" ++ fromString newfname ++ "\""
              else show ps)

    when (ps==PastaFile) $ do
      exists <- io $ doesFileExist newfname
      if exists
        then do
          cw <- postAsyncWhenPathModified newfname $
            runE e $ regeneratePastaGen mcw
          set mcw $ Just cw
        else do
          tempError 5 $
            "Файл с пастой не существует \"" ++ fromString newfname ++ "\""

    io . atomically . writeTVar (tpastagen shS) =<< generatePastaGen ps

pastaEnvPart :: Builder -> EnvPart
pastaEnvPart b = EP
    (\e c -> do
        pastaSet <- newIORef $ coPastaSet c
        pastaText <- newIORef $ coPastaText c

        wcheckescapeinv <- setir (coEscapeInv c)
                    =<< builderGetObject b castToCheckButton "checkescapeinv"
        wcheckescapewrd <- setir (coEscapeWrd c)
                    =<< builderGetObject b castToCheckButton "checkescapewrd"
        wcheckshuffle <- setir (coShuffleReposts c)
                    =<< builderGetObject b castToCheckButton "checkshuffle"
        wcheckrandomquote <- setir (coRandomQuote c)
                    =<< builderGetObject b castToCheckButton "checkrandomquote"
{-
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
-}

        wradiofromthread <-
            builderGetObject b castToRadioButton "radio-fromthread"
        wradiosym        <-
            builderGetObject b castToRadioButton "radio-symbol"
        wradiopastafile  <-
            builderGetObject b castToRadioButton "radio-pastafile"
        wradionopasta    <-
            builderGetObject b castToRadioButton "radio-nopasta"
        wradiowidget     <-
            builderGetObject b castToRadioButton "radio-widget"

        mclose <- newIORef Nothing

        wwindow       <-
                    builderGetObject b castToWindow "window-edit-pasta"
        wbuttonspawn  <-
                    builderGetObject b castToButton "button-edit-pasta"
        wbuttonapply  <-
                    builderGetObject b castToButton "button-edit-pasta-apply"
        wbuttoncancel <-
                    builderGetObject b castToButton "button-edit-pasta-cancel"
        wbuttonok     <-
                    builderGetObject b castToButton "button-edit-pasta-ok"
        wtextview     <-
                    builderGetObject b castToTextView "textview-edit-pasta"

        editorWidget
            (get pastaText)
            (\t -> runE e $ do
                writeLog $ "Set new pasta through widget: \"" ++ t ++ "\""
                set pastaText t
                regeneratePastaGen mclose)
            wwindow
            wtextview
            (Apply wbuttonapply)
            (Ok wbuttonok)
            (Cancel wbuttoncancel)
            (Spawn wbuttonspawn)

        let pastaradio =
                [(FromThread, wradiofromthread)
                ,(Symbol, wradiosym)
                ,(PastaFile, wradiopastafile)
                ,(NoPasta, wradionopasta)
                ,(FromWidget, wradiowidget)
                ]

        pwcei <- newIORef =<< get wcheckescapeinv
        pwcew <- newIORef =<< get wcheckescapewrd
        pwcsr <- newIORef =<< get wcheckshuffle
        let
          pastafileassoc =
            [(wcheckescapeinv, pwcei)
            ,(wcheckescapewrd, pwcew)
            ]
          pastafilewidgets =
            [{- wcheckescapeall
            , -}
             wcheckescapeinv
            ,wcheckescapewrd]

        let
          setSensitive t assoc senswidgets = do
            if t
              then
                forM_ assoc $ \(w, p) ->
                    toggleButtonSetActive w =<< get p
              else do
                forM_ assoc $ \(w, p) -> do
                    whenM (G.get w widgetSensitive) $
                        set p =<< get w
                    toggleButtonSetActive w False
            mapM_ (`widgetSetSensitive` t) senswidgets

        void $ flip anyM pastaradio $ \(p, w) ->
            if (p == coPastaSet c)
              then True <$ do
                toggleButtonSetActive w True
                setSensitive (p==PastaFile || p==FromWidget)
                    pastafileassoc pastafilewidgets
              else return False
{-
        let updAll = unlessM (get ignore) $ do
                set ignore True
                maybe (toggleButtonSetInconsistent wcheckescapeall True)
                      (\a -> do
                        toggleButtonSetInconsistent wcheckescapeall False
                        setIO wcheckescapeall a) =<<
                    bolall wcheckescapeinv wcheckescapewrd
                set ignore False
-}
        forM_ pastaradio $ \(p, w) -> do
            void $ on w toggled $
                whenM (toggleButtonGetActive w) $ do
                    writeIORef pastaSet p
                    runE e $ regeneratePastaGen mclose
                    setSensitive (p==PastaFile || p==FromWidget)
                        pastafileassoc pastafilewidgets

        wentrypastafile <- setir (coPastaFile c)
                            =<< builderGetObject b castToEntry "entrypastafile"
        wbuttonpastafile <- builderGetObject b castToButton "buttonpastafile"

        onFileChooserEntryButton False wbuttonpastafile wentrypastafile
            (runE e . tempError 3)
            (whenM ((==PastaFile) <$> readIORef pastaSet) $ do
                runE e $ regeneratePastaGen mclose)

        void $ on wcheckescapeinv buttonActivated $ do
            -- updAll
            runE e $ regeneratePastaGen mclose

        void $ on wcheckescapewrd buttonActivated $ do
            -- updAll
            runE e $ regeneratePastaGen mclose

        void $ on wcheckshuffle buttonActivated $ do
            runE e $ regeneratePastaGen mclose

        void $ on wcheckrandomquote buttonActivated $ do
            runE e $ regeneratePastaGen mclose

        void $ postAsyncWhenPathModified (coPastaFile c) $
            runE e $ regeneratePastaGen mclose

--        updAll

        postGUIAsync $ runE e $ regeneratePastaGen mclose

        return
            (pastaSet
            ,wentrypastafile
            ,wcheckescapeinv
            ,wcheckescapewrd
            ,pwcei
            ,pwcew
            ,wcheckshuffle
            ,pwcsr
            ,wcheckrandomquote
            ,pastaText
            )
        )
    (\(ps',wepf,wei,wew,pwei,pwew,wsr,pwsr,wcrq,ipt) c -> do
        ps <- get ps'
        pf <- get wepf
        -- if widget is active, use current value, else use the one which was
        -- set when it was active
        ei <- ifM (G.get wei widgetSensitive) (get wei) (get pwei)
        ew <- ifM (G.get wew widgetSensitive) (get wew) (get pwew)
        sr <- ifM (G.get wsr widgetSensitive) (get wsr) (get pwsr)
        rq <- get wcrq
        pt <- get ipt
        return c{coPastaSet=ps
                ,coPastaFile=pf
                ,coPastaText=pt
                ,coEscapeInv=ei
                ,coEscapeWrd=ew
                ,coShuffleReposts=sr
                ,coRandomQuote=rq})
    (\(ps,wepf,wcei,wcew,_,_,wcsr,_,wcrq,ipt) e ->
        e{pastaSet=ps
         ,pastaText=ipt
         ,wentrypastafile=wepf
         ,wcheckescapeinv=wcei
         ,wcheckescapewrd=wcew
         ,wcheckshuffle=wcsr
         ,wcheckrandomquote=wcrq
         })
