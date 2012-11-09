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
-- TODO benchmark fast-tagsoup
--      vs. tagstream-conduit → entities → conv-tagsoup-types (NOTE tagstream is not lazy, that won't work)
--      vs. regular expressions
-- FIXME Blast lazyness/strictness. Now that we lazily parse everything we run in constant space(?)
-- TODO We still can't set higher priority for thread with GUI, (perhaps we could through OS API...)
-- So it'll lag anyway, unless we move workers to different process.
-- FIXME We need a threadscope profile before we can decide on anything
-- TODO System.Random is abysmally slow, and might cause some lag on escaping. marsenne-random, mws-random?
-- FIXME http-conduit doesn't play well with AWS in uploader, spawn curl instead.
-- FIXME criterion fromString/drop vs. Text/drop, ghci +s doesn't use optimizations.

-- == 2.0 RELEASE ==
-- TODO Stable download links using github pages

-- URGENT TODO >Что за пиздец с «этот файл уже загружен»? Неужели трудно по умолчанию менять пару байт в картинке перед отправкой?

-- TODO avoid parsing page when only creating threads / when it's time time to create a thread and createthread probability == always
-- TODO avoid rolling a dice when createthread == always

-- WTF snoyman's requestTimeout creates a new haskell thread for every request
--     new http-conduit seems to consult system certificates even on non-https requests (certs too many open files)
--     it also appears that with HTTP package we can have many simultaneous threads with requests
--     without needing to link with threaded runtime, while http-conduit needs obligatory -threaded(?)
--     http-conduit seems to have a lot bigger memory consumption than HTTP, see OOM reports.(?)
--     CLARIFY wait, what'd happen if i put thousands of connections on one manager? [Nothing happens, at least nothing different from when you put thousands of connections on different managers]
--             >May be used concurrently by multiple threads.
--             seems to indicate that this is what we need.
--     CLARIFY Is there a memory/resource leak in void $ http (parseUrl "http://example.com")

-- WTF
--      "getAddrInfo: does not exist (Name or service not known)" — when connecting with more than 500 threads at the same time.(DNS antiDOS?)
--      "socket: resource exhausted (Too many open files)" — when connecting with more than 500 threads at the same time.
--      "/etc/ssl/certs/: getDirectoryContents: resource exhausted (Too many open files)" — http-conduit-1.8 regression, newManager/systemCertificate

-- TODO 403
-- TODO небампание определенных тредов (планета)
-- TODO Merge Blast and BlastLog, expose BlastLog. Merge tpastagen and timagegen into tpostdatagen.
-- TODO debuglog/normallog
-- TODO skipCaptcha только когда уже получен один проход без капчи
-- TODO show offending message in sameMessage and others
-- TODO показывать причину последнего бана когда все забанены
-- TODO DETECT CLOUDFLARE WHEN POSTING
-- TODO Abstract (hierarchical) config management in BlastItWithPiss
-- TODO cliblast, убрать тормоза
-- TODO Новый ключ антигейта + кошелек донатов
-- TODO Вайпать несколько тредов
-- TODO Не расходовать капчу зря
-- TODO АВТОМАТИЧЕСКОЕ ПЕРЕПОДКЛЮЧЕНИЕ
-- TODO Писать забаненные/сдохнувшие прокси в файл+(борда X причина/ексепшн)
-- TODO фильтровать забаненные / сдохнувшие.
-- TODO Перепостинг из других досок
-- TODO Настройка стратегии
-- TODO Записывать конфиг сразу, а не при закрытии.
-- TODO newscreen.jpg, oppost update, README/COMPILEGUIDE, ну вы понели
-- TODO Кэширование манифеста (ETag например)
-- TODO Убрать жуткую вытянутость по вертикали.
-- TODO Поставить запросы на постинг в очередь(avoid 503)
-- TODO avoid 403 ban
-- TODO Manual.md
-- TODO benchmark fast-tagsoup LByteString & LText
-- TODO оптимизировать ещё (прекратить пложение ОС-тредов? Реже парсить страницу?)

-- == FUTURE IMPROVEMENTS ==
-- TODO better exceptions for 404, 403, strange 303 wakabapl(TooFastPost?), cloudflare ban, detect cloudflare when posting, mochan down.
-- TODO Пикчи на тему ссания в жопу из Kuso Miso Technique.
-- TODO Configurable max_bid, sleepwait and sleepcaptcha
-- TODO GTK keyboard completion in board list (list view / table / ad-hoc)
-- TODO отображать состояние антигейта в updWipeMessage (add hook)
--      например количество капч решаемых в данный момент или stat.php
-- TODO add multipart/form-data to http-conduit
-- TODO support alternatives to antigate — CAPTCHABOT, DECAPTCHER etc.
-- TODO add zip file permissions to zip-archive
-- TODO make updater a standalone library and release on hackage?("crude-autoupdater.cabal", it'll need quite a generalization to fit as a general purpose library.)
-- TODO i18n (represent messages by types + typeclass?)
-- TODO config last thread time
-- TODO Показывать несколько капч одновременно
-- TODO background mode
-- TODO Support 2chnu, alterchan.
-- TODO add API as a fallback if can't parse html

-- == REFACTORING ==
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

    rawPutLog =<< (("Starting gtkblast. Version " ++ showVersion version ++ ". Current time is ") ++) . show <$> getZonedTime

    configfile <- (</> "config.json") <$> configDir

    conf <- readConfig configfile

    rawPutLog $ "Loaded config: " ++ show conf

    -- start

    handle (\(a::SomeException) -> do
            t <- getZonedTime
            rawPutLog $ "Uncaught exception terminated program. Current time is " ++ show t ++ "\nException was: " ++ show a
            exitFailure) $ do

        -- init

        void $ initGUI
        builder <- builderNew
        builderAddFromFile builder $ resourceFile "blast.glade"

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

        rawPutLog =<< ("Finished wipe session, current time is " ++) . show <$> getZonedTime

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
