module GtkBlast.Video
    (VideoSet(..)

    ,videoEnvPart

    ,emptyVideoGen
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
import BlastItWithPiss.Video
import BlastItWithPiss.MonadChoice

import System.Directory

import Graphics.UI.Gtk hiding (get,set)

import qualified Data.Text as T

import Control.Concurrent.STM

regenerateVideoGen :: IORef (Maybe CloseWatcher) -> E ()
regenerateVideoGen mcw = do
    maybe (return ()) closeWatcher =<< get mcw
    set mcw Nothing
    e@E{..} <- ask
    vs <- get videoSet
    newfname <- get wentryvideofile
    writeLog $ "Video changed \"" ++ show vs ++ ":" ++ fromString newfname ++ "\""
    when (vs == VideoFromFile) $ do
      exists <- io $ doesFileExist newfname
      if exists
        then do
          cw <- postAsyncWhenPathModified newfname $ runE e $ regenerateVideoGen mcw
          set mcw $ Just cw
        else do
          tempError 5 $ "Файл с видео не существует \"" ++ fromString newfname ++ "\""
    io . atomically . writeTVar (tvideogen shS) =<< generateVideoGen vs

emptyVideoGen :: TempGenType Text
emptyVideoGen = mkConstGen T.empty

generateVideoGen :: VideoSet -> E (TempGenType Text)
generateVideoGen VideoNothing =
    return $ mkConstGen T.empty
generateVideoGen VideoFromFile = do
    E{..} <- ask

    !videofile <- get wentryvideofile
    !videos <- appFile [] (fmap (fromMaybe []) . readVideoFile) videofile

    return $ mkIgnoreGen $ fromMaybe "" <$> chooseFromListMaybe videos
generateVideoGen VideoFromWidget = do
    E{..} <- ask

    !videos <- fromMaybe [] . parseVideo <$> get videoText

    return $ mkIgnoreGen $ fromMaybe "" <$> chooseFromListMaybe videos

videoEnvPart :: Builder -> EnvPart
videoEnvPart b = EP
    (\e c -> do
        videoSet <- newIORef $ coVideoSet c
        videoText <- newIORef $ coVideoText c

        wradiovideonothing <- builderGetObject b castToRadioButton "radio-video-nothing"
        wradiovideofile <- builderGetObject b castToRadioButton "radio-video-file"
        wradiovideowidget <- builderGetObject b castToRadioButton "radio-video-widget"

        mclose <- newIORef Nothing

        wwindow <- builderGetObject b castToWindow "window-edit-video"
        wbuttonspawn <- builderGetObject b castToButton "button-edit-video"
        wbuttonapply <- builderGetObject b castToButton "button-edit-video-apply"
        wbuttoncancel <- builderGetObject b castToButton "button-edit-video-cancel"
        wbuttonok <- builderGetObject b castToButton "button-edit-video-ok"
        wtextview <- builderGetObject b castToTextView "textview-edit-video"
        editorWidget
            (get videoText)
            (\t -> runE e $ do
                writeLog $ "Set new video through widget: \"" ++ t ++ "\""
                set videoText t
                regenerateVideoGen mclose)
            wwindow
            wtextview
            (Apply wbuttonapply)
            (Ok wbuttonok)
            (Cancel wbuttoncancel)
            (Spawn wbuttonspawn)

        let videoradio =
                [(VideoNothing, wradiovideonothing)
                ,(VideoFromFile, wradiovideofile)
                ,(VideoFromWidget, wradiovideowidget)
                ]

        void $ flip anyM videoradio $ \(p, w) ->
            if (p == coVideoSet c)
                then True <$ toggleButtonSetActive w True
                else return False

        forM_ videoradio $ \(v, w) -> do
            void $ on w toggled $
                whenM (toggleButtonGetActive w) $ do
                    set videoSet v
                    runE e $ regenerateVideoGen mclose

        wentryvideofile <- setir (coVideoFile c) =<< builderGetObject b castToEntry "entryvideofile"
        wbuttonvideofile <- builderGetObject b castToButton "buttonvideofile"

        onFileChooserEntryButton False wbuttonvideofile wentryvideofile
            (runE e . tempError 3)
            (whenM ((==VideoFromFile) <$> readIORef videoSet) $ do
                runE e $ regenerateVideoGen mclose)

        void $ postAsyncWhenPathModified (coVideoFile c) $
            runE e $ regenerateVideoGen mclose

        postGUIAsync $ runE e $ regenerateVideoGen mclose

        return
            (videoSet
            ,videoText
            ,wentryvideofile
            )
        )
    (\(vs', vt', wevf) c -> do
        vs <- get vs'
        vt <- get vt'
        vf <- get wevf
        return c
            {coVideoSet = vs
            ,coVideoText = vt
            ,coVideoFile = vf})
    (\(vs, vt, wevf) e ->
        e{videoSet=vs
         ,videoText=vt
         ,wentryvideofile=wevf})

