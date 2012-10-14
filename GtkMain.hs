module Main (main) where
import Import hiding (on, mod)
import GtkBlast.Directory
import GtkBlast.Log
import GtkBlast.Conf
import GtkBlast.EnvParts (createWidgetsAndFillEnv)
import GtkBlast.Mainloop (setMainLoop)
import Graphics.UI.Gtk hiding (get)
import System.Environment.UTF8
import System.Exit
import System.FilePath
import Network (withSocketsDo)
#ifdef BINDIST
import System.Directory (setCurrentDirectory)
import System.Environment.Executable (splitExecutablePath)
#endif
import GtkBlast.ROW_ROW_FIGHT_THE_POWER

-- TODO обращать внимание на бамплимит в вайпе
-- TODO avoid parsing page if mthread is set

-- TODO Tagsoup is the source of freezes, parseTags allocates a shitton
-- CLARIFICATION dropped in favor of fast-tagsoup
-- TODO benchmark fast-tagsoup vs. tagstream-conduit → entities → conv-tagsoup-types (NOTE tagstream is not lazy, that won't work)
-- TODO add API as a fallback if can't parse html
-- FIXME Blast lazyness/strictness. Now that we lazily parse everything we run in constant space(?)

-- TODO FIXME FIXME readIORef buBanned
-- TODO don't regenerate banned threads
-- TODO don't regenerate threads until asked to.
-- TODO убирать капчу от дохлых тредов
-- TODO better exceptions for 404 ban

-- TODO Обход вордфильтра — автобан. Это фича, сделать отдельную кнопку.
-- TODO mochepasta resources/mocha, change default boards, newscreen.jpg, repo description, README
-- TODO Sane defaults.
-- TODO Updater
-- TODO proxy checker is now useless, bundle it, but don't advertise.
-- TODO helpMessage
-- TODO реклама вайпалки в самом вайпе (в отдельном файле advertisement, постится и при садизме и при моче)
--      и соответствующая опция для отключения рекламы вайпалки
--      + реклама картинкой
-- TODO Выскакивать попап о том куда писать баг-репорты, о том что любой фидбек
--      , даже "я посрал" — приветствуется.
--      И о том что если вы забанены или кажется что что-то не так, то можно
--      перезапустить вайпалку (с BlastItWithPiss(.exe), а не blastgtk(.exe))
--      и посмотреть есть ли апдейты (Когда апдейтер будет готов)
-- TODO Фотожабы на тему ссания в жопу, из Kuso Miso Technique.
-- TODO Configurable max_bid, sleepwait and sleepcaptcha
-- TODO вайп отдельных тредов, конфигурация сажи, настройка стратегий
-- TODO АВТОМАТИЧЕСКОЕ ПЕРЕПОДКЛЮЧЕНИЕ

-- TODO Replace (OriginStamp, Message) with appropriate type, replace Message(SendCaptcha) with dedicated type
-- TODO Move more envparts from EnvParts.hs to their own modules
-- TODO Switch to immutable state, don't modify environment from widgets, send events instead.
-- TODO Add more type safety.(Any type safety?)
-- TODO Move ssach/recaptcha/cloudflare-specific functionality to their own modules
-- FIXME Кажется за каждый reverse мне светит по ебалу
-- TODO cleanup
-- TODO document

-- TODO make updater a standalone library and release on hackage?("crude-autoupdater.cabal", it'll need quite a generalization to fit as a general purpose library.)
-- TODO add blastcli
-- TODO zip file permissions
-- TODO отображать состояние антигейта в updWipeMessage (add hook)
--      например количество капч решаемых в данный момент или stat.php
-- TODO support alternatives to antigate — CAPTCHABOT, DECAPTCHER etc.
-- TODO get a hackage account and release antigate
-- TODO GTK keyboard completion in board list
-- TODO update description when snoyman releases http-conduit-1.7.0
-- TODO add multipart/form-data to http-conduit
-- TODO i18n (represent messages by types + typeclass?)
-- TODO configurable escaping
-- TODO configurable timeout
-- TODO config last thread time
-- TODO Показывать несколько капч одновременно
-- TODO background mode
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

    configfile <- (</> "config.json") <$> configDir

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

        setMainLoop env configfile setConf

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
