{-# LANGUAGE NoImplicitPrelude #-}
-- FIXME FIXME FIXME
{-# LANGUAGE ExistentialQuantification #-}
-- FIXME FIXME FIXME
module Main where
import Import hiding (on, mod)
import GtkBlast.IO
import GtkBlast.MuVar
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Pasta
import GtkBlast.Image
import GtkBlast.Proxy
import GtkBlast.Conf
import GtkBlast.MutableConfigurableWidgets
import GtkBlast.Maintenance
import Graphics.UI.Gtk hiding (get)
import GHC.Conc
import Control.Concurrent.STM
import System.Environment.UTF8
import System.Exit
import System.FilePath
import Network (withSocketsDo)
#ifdef BINDIST
import System.Directory (setCurrentDirectory)
import System.Environment.Executable (splitExecutablePath)
#endif
import GtkBlast.ROW_ROW_FIGHT_THE_POWER

-- TODO don't regenerate banned threads
-- TODO FIXME FIXME readIORef buBanned

-- TODO прокси
-- TODO support ANTIGATE, CAPTCHABOT, DECAPTCHER etc.
-- TODO вайпать постами из треда/страницы choosePost
-- TODO Move RandomNum/RandomChar generation to worker threads
-- TODO don't escape RandomNum and RandomChar.
-- TODO don't regenerate banned
-- TODO switch to JSON for config and manifest
-- TODO fix conf default boards
-- TODO Updater
-- TODO use appendFile,  don't output anything on shindos
-- TODO proxy checker is now useless, bundle it, but don't advertise.
-- TODO helpMessage
-- TODO реклама вайпалки в самом вайпе (в отдельном файле advertisement, постится и при садизме и при моче)
--      и соответствующая опция для отключения рекламы вайпалки
-- TODO mochepasta resources/mocha, change default boards
-- TODO Выскакивать попап о том куда писать баг-репорты, о том что любой фидбек
--      , даже "я посрал" — приветствуется.
--      И о том что если вы забанены или кажется что что-то не так, то можно
--      перезапустить вайпалку (с BlastItWithPiss(.exe), а не blastgtk(.exe))
--      и посмотреть есть ли апдейты (Когда апдейтер будет готов)
-- TODO update mocha-repo description

-- TODO breakup everything into modules with explicit export lists
-- TODO split processing from execution
-- TODO cleanup
-- TODO document
-- TODO Add more type safety.(Any type safety?)

-- TODO GTK keyboard completion in board list
-- TODO АВТОМАТИЧЕСКОЕ ПЕРЕПОДКЛЮЧЕНИЕ
-- TODO update description when snoyman releases http-conduit-1.7.0
-- TODO add multipart/form-data to http-conduit
-- TODO add API as a fallback if can't parse html
-- TODO don't regenerate threads until asked to.
-- TODO configurable escaping
-- TODO configurable timeout
-- TODO config last thread time
-- TODO Показывать несколько капч одновременно
-- TODO background mode
-- TODO FIX FREEZES
-- TODO Move ssach/recaptcha/cloudflare-specific functionality in their own modules
-- TODO Support 2chnu, alterchan.

bugMessage :: String
bugMessage = "If you experience crashes, bugs, or any kind strange or illogical behavior,"
          ++ " file a bug report to the author(https://github.com/exbb2/BlastItWithPiss/issues)"
          ++ " with attached files log.txt, and, if you have one, log.txt.bak.\n"
          ++ "Thanks, and have fun. Hopefully, it has been worth the weight."

helpMessage :: String
helpMessage = "No help message for now, sorry\n\n" ++ bugMessage

mainloop :: E ()
mainloop = do
    E{..} <- ask
    whenM (get wipeStarted) $ do
        io $ progressBarPulse wprogresswipe
        regeneratePasta
        regenerateImages
        regenerateProxies
        maintainBoardUnits
        updWipeMessage
        outs <- io $ atomically $ untilNothing $ tryReadTQueue tqOut
        forM_ outs reactToMessage

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    when (any (`elem` args) ["--help", "-h", "-?"]) $ do
       putStrLn helpMessage
       exitSuccess
    
     -- change workdir
#ifdef BINDIST
    (path, _) <- splitExecutablePath
    setCurrentDirectory path
#endif
    -- read configuration
  
    rawPutLog =<< ("Starting blastgtk. Current POSIX time is " ++) . show <$> getPOSIXTime
  
    configfile <- (</> "config") <$> configDir
  
    conf <- do
        x <- try $ readFile $ configfile
        case x of
            Left (a::SomeException) -> do
                rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" , loading defaults. Exception was: " ++ show a
                return def
            Right c ->
                case readMay c of
                    Nothing -> do
                        let confold = configfile <.> "old.faulty"
                        rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" because of syntax error, overwriting with defaults. Old version saved at \"" ++ confold ++ "\""
                        fromIOEM (return ()) $
                            writeFile confold c
                        return def
                    Just n -> return n
  
    rawPutLog $ "Loaded config: " ++ show conf
  
    -- start
  
    handle (\(a::SomeException) -> do
              rawPutLog $ "Uncaught exception terminated program, sorry: " ++ show a
              throwIO a) $ do
 
        -- init
    
        void $ initGUI
        builder <- builderNew
        builderAddFromFile builder =<< getResourceFile "blast.glade"
    
        -- setup environment
    
        (env, setConf) <- mkAllWidgets builder conf
    
        -- start main loop
    
        void $ timeoutAdd
            (do runE env mainloop
                return True) 50 --kiloseconds, 20 fps.
    
        void $ onDestroy (window env) $ runE env $ do
            writeConfig configfile setConf
            io $ mainQuit
    
        -- start main gui
    
        i am playing the game
        the one that'll take me to my end
        i am waiting for the rain
        to wash up who i am
    
        libera me from $osach:
            DO THE IMPOSSIBLE!
            SEE THE INVISIBLE!
            ROW! ROW!
            FIGHT THE POWER!
            
            TOUCH THE UNTOUCHABLE!
            BREAK THE UNBREAKABLE!
            ROW! ROW!
            FIGHT THE POWER!
            
            ROW! ROW!
            FIGHT THE POWER!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             [you lost The Game]
  
        -- say good bye
 
        rawPutLog =<< ("Finished wipe session, current POSIX time is " ++) . show <$> getPOSIXTime

mochanNames :: [String]
mochanNames =
    ["мочан"
    ,"сосач"
    ,"ссач"
    ,"педальчан"
    ,"уринач"
    ,"мочеиспускач"
    ,"абучан"
    ,"двасо"
    ,"хачан"
    ,"мочепарашу"
    ,"мочепарашу 2ch.so"
    ,"педальный обоссач"
    ,"педальный уринач"
    ,"педальный абучан"
    ,"педальный хачан"
    ,"педальную мочепарашу"
    ,"педальную мочепарашу 2ch.so"
    ,"уринальный мочеиспускач"
    ,"уринальный абучан"
    ,"уринальный хачан"
    ,"уринальную мочепарашу"
    ,"уринальную мочепарашу 2ch.so"
    ,"трипфажный обоссач"
    ,"трипфажный мочан"
    ,"трипфажный мочеиспускач"
    ,"трипфажный абучан"
    ,"трипфажную мочепарашу"
    ,"трипфажную мочепарашу 2ch.so"
    ]
