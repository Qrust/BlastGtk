module Main (main) where
import Import hiding (on, mod)

import Paths_blast_it_with_piss

import GtkBlast.Directory
import GtkBlast.Log
import GtkBlast.Conf
import GtkBlast.EnvParts (createWidgetsAndFillEnv)
import GtkBlast.Mainloop (setMainLoop)
import GtkBlast.ROW_ROW_FIGHT_THE_POWER

import Graphics.UI.Gtk hiding (get)

import qualified Data.Text as T

import Data.Version

import System.Environment (getArgs)
import System.Exit

import System.FilePath

import System.IO (putStrLn)

#ifdef BINDIST
import System.Directory (setCurrentDirectory)
import System.Environment.Executable (splitExecutablePath)
#endif

import Network (withSocketsDo)




{- URGENT
    Loop / race condition stopping
    _the GUI thread_, AND _also stopping any wipe_ introduced somewhere
    between 1.1.22 5ee80c7cd96fb16a6becdb010d9f8a3da4874f8f (2013-03-18 00:18:31)
    and 1.1.23 065d3a2e75db920573bbdfff24abc39729c962ef (2013-03-18 13:09:33)
    Log: dlinynos-log-smaller_log.txt
    Candidates:
      - Not blastCloudflare # not in log
      - not a letrec?       # consumes 100% CPU
      - Antigate captcha    # antigate figures in log
      - Log                 # very large log
      - EnvPart             # the gui stops
-}
-- URGENT share parse results perBoard
-- URGENT numCapabilities?
-- URGENT --exclude

-- URGENT Checker with captcha
-- URGENT Presolve & migrate captcha (structure captcha solving as a (?term)resource pool/conduit)
-- URGENT Thread pool
-- URGENT observer threads instead of regens

-- URGENT smyvalka write Log
-- URGENT smyvalka show captchas currently in solving

-- URGENT Avoid reopening log handle
-- URGENT Use FastLogger/ByteString for log

-- URGENT CaptchaAnswerWithReport type, CaptchaSolver class
--        ,Generalize BlastItWithPiss to allow it to be used in checker and smyvalka
--        (return sleep time, instead of sleeping, to allow for thread pool?)
-- URGENT AdaptiveIn is a property of board+proxy not of a single agent
-- URGENT ввод капчи руками в смывалке и чекере

-- URGENT Glade destroys tooltips on cut/paste. Move setting tooltips to code.

-- URGENT global useragent makes all proxies share useragent
--        Rid of unsafePerformIO in Blast.userAgent, user-agent is a proxy property
-- URGENT fix longposting (image)
-- URGENT don't label non-modded threads as CreateNew







-- https://github.com/blog/1302-goodbye-uploads
--
-- dropbox, git-annex?
-- bitbucket has Downloads section, we can setup a mirror there.
-- google code and sourceforge also have Downloads sections








-- == PERFORMANCE ==
-- TODO benchmark fast-tagsoup
--      vs. tagstream-conduit → entities → conv-to-tagsoup-types (NOTE tagstream is not lazy, that won't work)
--      vs. regular expressions
-- TODO benchmark fast-tagsoup LByteString & LText vs. ByteString & Text
-- FIXME Blast lazyness/strictness. Now that we lazily parse everything we run in constant space(?)
-- TODO We still can't set higher priority for thread with GUI, (perhaps we could through OS API...)
--      So it'll lag anyway, unless we move workers to different process.
-- TODO System.Random is slow, and might cause some lag on escaping. marsenne-random, mws-random?
-- FIXME criterion fromString/drop vs. Text/drop, ghci +s doesn't use optimizations.
-- CLARIFY Does Text leaks on drop? (seems from the source that data before the substring is not GC'd, CLARIFY)

-- TODO http-conduit:
--  Unify proxy types / Define proxy chains
--  Cookie filter / map, cookie helpers
--  DNS caching

-- FIXME Is there a memory/resource leak in void $ http (parseUrl "http://example.com")?
--       There is, see https://github.com/snoyberg/http-conduit/issues/97#issuecomment-12858211

-- == 2.0 RELEASE ==

-- URGENT To add a new option you need to edit at least 3 files.
--  This is the worst peace of software i heard of.

-- URGENT post videos
-- URGENT synchronize pastas and images/videos
-- TODO shuffleM in pastagen is probably ungodly slow

-- URGENT Some lag in the GUI MIGHT be caused by unevaluated thunks in stm stuff.

-- URGENT JunkImage type
-- URGENT image upload via conduit source
-- URGENT Fix proxychecker

-- URGENT капча на несколько постов вперед.
-- URGENT repeat on 503, database error, once on http

-- URGENT Настройка темы, поле имя/трипкод, видео
-- URGENT Разделить цитату номера поста и цитату содержимого
-- URGENT табовый интерфейс
-- URGENT вставлять ссылку на тред, не только номер
-- URGENT blastitwithpiss.github.com

-- URGENT
--  Синхронизация юнитов, кэширование страниц(не только нулевой) между проксями
--  на одной борде. Записывать какие треды бампнули с штампом чтобы не бампать
--  дважды. Записывать какие треды создали с ноко, чтобы бампать вайп;
--  BumpUnpopular → BumpWipe, otherwise BumpOld. Перезаргузка некэшированных
--  страниц.
--  + Слежение за тредом, автобамп
-- URGENT Rename EnvPart → Widget
-- URGNET blastCloudflare → withCloudflare
-- URGENT ControlCenter type/class which controls wipe units, WipeUnit type.
-- URGENT Remove "{" ++ "}"
-- URGENT parse less often if board is slow

-- URGENT MANUAL + corner cases (403, wordfilter, etc.) + use cases (засирание треда, вайп борды, смыв, закос под ручной вайп, автобамп)
-- URGENT Five'o'Three BlastItWithPiss workaround
-- URGENT Outcome 403Ban
-- URGENT Обход клаудфлера при постинге & клаудфлер в смывалке
-- URGENT Поставить запросы на постинг в очередь(avoid wakaba.pl 503)
-- URGENT Better error messages (no parse, 403, etc.)
-- URGENT GHC under Wine HaskellWiki

--Smyvalka
-- FIXME Капча почему-то привязана к проксям, но ведь это общий пул.

--BlastItWithPiss lib:
-- TODO Безжалостное уродование текста (замена на цифры, leet, вставка пробелов/минусов)
-- TODO avoid parsing page when only creating threads / when it's time to
--      create a thread and createthread == always
-- TODO avoid rolling a dice when createthread == always
-- URGENT >Что за пиздец с «этот файл уже загружен»? Неужели трудно по умолчанию менять пару байт в картинке перед отправкой?
--      УМВР вроде.
--      Может добавлять мусор ещё и в начало?
-- TODO Смывание доски с собиранием капчи
-- TODO skipCaptcha только когда уже получен один проход без капчи
-- TODO Не расходовать капчу зря
-- TODO avoid 403 ban
-- TODO небампание определенных тредов (планета)
-- TODO Merge Blast and BlastLog, expose BlastLog. Merge tpastagen and timagegen into tpostdatagen. debuglog/normallog
-- TODO show offending message in SameMessage and others
-- TODO DETECT CLOUDFLARE WHEN POSTING
-- TODO Abstract out (hierarchical) config management in BlastItWithPiss
-- TODO Реже парсить страницу.
-- TODO Share parsed page between all proxies working on the same board.
-- TODO Перепостинг из других досок
-- TODO оптимизировать ещё (прекратить пложение ОС-тредов? fix network synchronous)
-- TODO Remove smyvalka (+Updater.Repair)

--GtkMain:
-- TODO BlastThreadId.
-- TODO cliblast, убрать тормоза
-- TODO показывать причину последнего бана когда все забанены
-- TODO Записывать конфиг сразу, а не только при закрытии.
-- TODO Вайпать несколько тредов
-- TODO АВТОМАТИЧЕСКОЕ ПЕРЕПОДКЛЮЧЕНИЕ
-- TODO Останавливать вайп и показывать ачивку после нескольких безуспешных переподключений. ("Вы охуенны, ваш титул «%s».\nЗаскрините, покажите друзьям, сосните хуйца."
-- TODO считать баны, включать в ачивки
-- TODO Писать забаненные/сдохнувшие прокси в файл+(борда X причина/ексепшн)
-- TODO фильтровать забаненные / сдохнувшие.
-- TODO Настройка стратегии
-- TODO Убрать жуткую вытянутость по вертикали.

--Updater:
-- TODO Кэширование манифеста (ETag например)

--Other:
-- TODO перечислить такие-то фичи
-- TODO Новый ключ антигейта + кошелек донатов
-- TODO newscreen.jpg, oppost update, README/COMPILEGUIDE, ну вы понели
-- TODO Manual.md
-- TODO Stable download links using github pages

-- == FUTURE IMPROVEMENTS ==
-- TODO better exceptions for 404, 403, strange 303 wakabapl(TooFastPost?), cloudflare ban, detect cloudflare when posting, mochan down.
-- TODO Пикчи на тему ссания в жопу из Kuso Miso Technique.
-- TODO Configurable max_bid, sleepwait and sleepcaptcha
-- TODO GTK keyboard completion in board list (list view / table / ad-hoc)
-- TODO отображать состояние антигейта в updWipeMessage (add hook)
--      например количество капч решаемых в данный момент или stat.php
-- TODO add multipart/form-data to http-conduit
-- TODO add zip file permissions to zip-archive
-- TODO make updater a standalone library and release on hackage?("crude-autoupdater.cabal", it'll need quite a generalization to fit as a general purpose library.)
-- TODO i18n (represent messages by types + typeclass?)
-- TODO config last thread time
-- TODO Показывать несколько капч одновременно
-- TODO background mode
-- TODO Support 2chnu, alterchan.
-- TODO add API as a fallback if can't parse html
-- TODO Постинг в /o/
-- TODO лучше маскировка убрать mfod

-- == REFACTORING ==
-- TODO Replace (OriginStamp, Message) with appropriate type, replace Message(SendCaptcha) with dedicated type, add a type for CompactStamp
-- TODO Move more envparts from EnvParts.hs to their own modules
-- TODO ugliest things: regenerations, "old" vars in disableable envparts.
-- TODO Being functional means modeling a program in a data-oriented fashon, not
--      effect-oriented. Right now BlastItWithPiss is as imperative as it gets.
-- FIXME EnvPart internal state shouldn't really lie around in global env?
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

    _t <- show <$> getZonedTime
    putInvisibleLog $
        "Starting gtkblast. Version " ++ T.pack (showVersion version) ++
        ". Current time is " ++ _t

    configfile <- (</> "config.json") <$> configDir
    conf <- readConfig configfile
    putInvisibleLog $ "Loaded config: " ++ show conf

    -- start

    handle (\(e::SomeException) -> do
        t <- getZonedTime
        putInvisibleLog $
            "Uncaught exception terminated program. Current time is "
            ++ show t ++ "\nException was: " ++ show e
        exitFailure) $ do

        -- init

        void $ initGUI
        builder <- builderNew
        builderAddFromFile builder $ bundledFile "resources/blast.glade"

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

        putInvisibleLog =<< ("Finished wipe session, current time is " ++) . show <$> getZonedTime
