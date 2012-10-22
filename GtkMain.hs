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
import Paths_blast_it_with_piss
import Data.Version

--- /\  FIXMES /\
-- /==\ TODOS /==\
-- \==/ NOTES \==/
--  \/  PLANS  \/

-- == PERFORMANCE ==
-- TODO Tagsoup is the source of freezes, parseTags allocates a shitton
-- CLARIFICATION dropped in favor of fast-tagsoup
-- TODO benchmark fast-tagsoup vs. tagstream-conduit → entities → conv-tagsoup-types (NOTE tagstream is not lazy, that won't work)
-- TODO add API as a fallback if can't parse html
-- FIXME Blast lazyness/strictness. Now that we lazily parse everything we run in constant space(?)
-- TODO We still can't set higher priority for thread with GUI, (perhaps we could through OS API...)
-- So it'll lag anyway, unless we move workers to different process.
-- FIXME We need a threadscope profile before we can decide on anything
-- TODO System.Random is abysmally slow, and might cause some lag on escaping. marsenne-random, mws-random?
-- FIXME http-conduit doesn't play well with AWS in uploader, spawn curl instead.

-- == 1.0 RELEASE ==
-- TODO change default boards, newscreen.jpg, repo description, README, sane defaults
-- TODO agitka.png, Охуенный ОП-пост + ОП-пикчи для разных борд.

-- == FUTURE IMPROVEMENTS ==
-- TODO АВТОМАТИЧЕСКОЕ ПЕРЕПОДКЛЮЧЕНИЕ
-- TODO better exceptions for 404, 403, strange 303 wakabapl(TooFastPost? Add PostRejected outcome?), cloudflare ban, detect cloudflare when posting, mochan down.
-- TODO Фотожабы на тему ссания в жопу из Kuso Miso Technique.
-- TODO вайп отдельных тредов, конфигурация сажи, настройка стратегий
-- TODO Configurable max_bid, sleepwait and sleepcaptcha
-- TODO GTK keyboard completion in board list (list view / table / ad-hoc)
-- TODO отображать состояние антигейта в updWipeMessage (add hook)
--      например количество капч решаемых в данный момент или stat.php
-- TODO support alternatives to antigate — CAPTCHABOT, DECAPTCHER etc.
-- TODO add blastcli
-- TODO add zip file permissions to zip-archive
-- TODO add multipart/form-data to http-conduit
-- TODO get a hackage account and release antigate
-- TODO make updater a standalone library and release on hackage?("crude-autoupdater.cabal", it'll need quite a generalization to fit as a general purpose library.)
-- TODO drop dependency on custom http-conduit when http-conduit-browser will be released(never?)
-- TODO i18n (represent messages by types + typeclass?)
-- TODO configurable timeout
-- TODO config last thread time
-- TODO Показывать несколько капч одновременно
-- TODO background mode
-- TODO Support 2chnu, alterchan.

-- == REFACTORING ==
-- TODO Merge Blast and BlastLog, expose BlastLog. Merge tpastagen and timagegen into tpostdatagen.
-- TODO Replace (OriginStamp, Message) with appropriate type, replace Message(SendCaptcha) with dedicated type, add a type for CompactStamp
-- TODO Move more envparts from EnvParts.hs to their own modules
-- TODO Switch to immutable state, don't modify environment from widgets, send events instead.
-- TODO Add more type safety.(Any type safety?)
-- TODO Add more modularity.(Any modularity?)
-- TODO Move ssach/recaptcha/cloudflare-specific functionality to their own modules
-- FIXME Escaping.hs: Кажется за каждый reverse мне светит по ебалу
-- TODO cleanup
-- TODO document

helpMessage :: String
helpMessage =
    "Единственная в своем классе вайпалка сосача, с няшным гуи и автообновлением. Написано на хачкеле.\n" ++
    "Справочный материал разбросан по тултипам, наводите мышку на интересующие вас элементы. Если вам нужна помощь обращайтесь в соответствующий тред на вашей доске, или в треды указанные на странице репозитория.\n" ++
    "https://github.com/exbb2/BlastItWithPiss\n" ++
    "Version: " ++ showVersion version

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    when (any (`elem` args) ["--help", "-h", "-?"]) $ do
       putStrLn helpMessage
       exitSuccess
    when (any (`elem` args) ["-V", "--version"]) $ do
       putStrLn $ showVersion version
       exitSuccess

     -- change workdir
#ifdef BINDIST
    (path, _) <- splitExecutablePath
    setCurrentDirectory path
#endif
    -- read configuration

    rawPutLog =<< (("Starting blastgtk. Version " ++ showVersion version ++ ". Current POSIX time is ") ++) . show <$> getPOSIXTime

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
