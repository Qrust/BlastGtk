{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Import hiding (on, mod)
import GtkBlast.IO
import GtkBlast.Directory
import GtkBlast.Environment
import GtkBlast.Log
import GtkBlast.Conf
import GtkBlast.EnvParts (createWidgetsAndFillEnv)
import GtkBlast.Mainloop (setMainLoop)
import Graphics.UI.Gtk hiding (get)
import System.Environment.UTF8
import System.Exit
import System.FilePath
import Network (withSocketsDo)
import Paths_blast_it_with_piss
#ifdef BINDIST
import System.Directory (setCurrentDirectory)
import System.Environment.Executable (splitExecutablePath)
#endif
import GtkBlast.ROW_ROW_FIGHT_THE_POWER

-- TODO Tagsoup is the source of freezes, parseTags allocates a shit ton
-- and forces program into 90% GC routine. Drop in favor of (html-conduit(cursor)/tagstream-conduit/xml-conduit(?))
-- Also parse API with those
-- FIXME Blast lazyness/strictness. Now that we force everything we run in constant space(?)
-- FIXME Text constants increase binary size a lot (4.2 mb, before Text — 3.9 mb) — investigate
-- TODO Обход вордфильтра — автобан. Это фича, сделать отдельную кнопку.

-- FIXME Compiling under wine is slow as hell, move haskell platform from NTFS to /opt.

-- TODO FIXME FIXME readIORef buBanned
-- TODO don't regenerate banned threads
-- TODO don't regenerate threads until asked to.

-- TODO вайпать постами из треда/страницы choosePost
-- TODO Move RandomNum/RandomChar generation to worker threads
-- TODO don't escape RandomNum and RandomChar.
-- TODO отображать состояние антигейта в updWipeMessage (add hook)
--      например количество капч решаемых в данный момент или stat.php
-- TODO switch to JSON for config and manifest
-- TODO mochepasta resources/mocha, change default boards
-- TODO Updater
-- TODO proxy checker is now useless, bundle it, but don't advertise.
-- TODO Выбор собственной пасты, убрать анальный садизм
-- TODO helpMessage
-- TODO реклама вайпалки в самом вайпе (в отдельном файле advertisement, постится и при садизме и при моче)
--      и соответствующая опция для отключения рекламы вайпалки
-- TODO Выскакивать попап о том куда писать баг-репорты, о том что любой фидбек
--      , даже "я посрал" — приветствуется.
--      И о том что если вы забанены или кажется что что-то не так, то можно
--      перезапустить вайпалку (с BlastItWithPiss(.exe), а не blastgtk(.exe))
--      и посмотреть есть ли апдейты (Когда апдейтер будет готов)
-- TODO Configurable max_bid, sleepwait and sleepcaptcha

-- TODO Replace (OriginStamp, Message) with appropriate type
-- TODO Switch to immutable state, don't modify state from widgets, send events instead.
-- TODO Add more type safety.(Any type safety?)
-- TODO cleanup
-- TODO document

-- TODO АВТОМАТИЧЕСКОЕ ПЕРЕПОДКЛЮЧЕНИЕ
-- TODO support alternatives to antigate — CAPTCHABOT, DECAPTCHER etc.
-- TODO GTK keyboard completion in board list
-- TODO update description when snoyman releases http-conduit-1.7.0
-- TODO add multipart/form-data to http-conduit
-- TODO add API as a fallback if can't parse html
-- TODO i18n (represent messages by types + typeclass?)
-- TODO configurable escaping
-- TODO configurable timeout
-- TODO config last thread time
-- TODO Показывать несколько капч одновременно
-- TODO background mode
-- TODO Move ssach/recaptcha/cloudflare-specific functionality to their own modules
-- TODO Support 2chnu, alterchan.

bugMessage :: String
bugMessage = "If you experience crashes, bugs, or any kind strange or illogical behavior,"
          ++ " file a bug report to the author(https://github.com/exbb2/BlastItWithPiss/issues)"
          ++ " with attached file log.txt.\n"
          ++ "Thanks, and have fun. Hopefully, it has been worth the weight."

helpMessage :: String
helpMessage = "No help message for now, sorry\n\n" ++ bugMessage

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
  
    conf <- readConfig configfile
  
    rawPutLog $ "Loaded config: " ++ show conf
  
    -- start
  
    handle (\(a::SomeException) -> do
              rawPutLog $ "Uncaught exception terminated program, sorry: " ++ show a
              exitFailure) $ do
 
        -- init
    
        void $ initGUI
        builder <- builderNew
        builderAddFromFile builder =<< getResourceFile "blast.glade"
    
        (env, setConf) <- createWidgetsAndFillEnv builder conf
    
        -- start main loop

        setMainLoop env
    
        void $ onDestroy (window env) $ runE env $ do
            writeConfig configfile =<< io (setConf def{coFirstLaunch=False, coLastVersion=version})
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
