module Main where
import Import
import "blast-it-with-piss" BlastItWithPiss.Post
import "blast-it-with-piss" BlastItWithPiss.Parsing
import "blast-it-with-piss" BlastItWithPiss.Blast
import "blast-it-with-piss" BlastItWithPiss.Board
import "blast-it-with-piss" BlastItWithPiss.MultipartFormData
import System.Console.CmdArgs.Implicit hiding (def)
import Control.Concurrent
import System.Environment
import Network.Socket
import System.IO.UTF8 (readFile, writeFile, appendFile)
import System.IO (print, putStrLn)

data Config = Config
    {socks :: Bool
    ,append :: Bool
    ,quiet :: Bool
    ,atatime :: Int
    ,timeout :: Int
    ,strBoard :: String
    ,thread :: Int
    ,input :: String
    ,output :: String
    ,banned :: String
    ,four'o'four :: String
    ,cloudflare_captcha :: String
    ,cloudflare_ban :: String
    ,bad :: String
    }
    deriving (Show, Data, Typeable)

impureAnnotatedCmdargsConfig :: Config
impureAnnotatedCmdargsConfig =
    Config
        {socks = False &= help "Файл с проксями содержит Socks5 прокси?(По моим тестам socks не работают, но я не уверен точно. Если у вас чекер работает с проксями или фейлится, но они работают в бровзере — пишите в тред)"
        ,append = False &= help "Если уже есть файлы с таким именем не перезаписывать их, а добавлять к ним"
        ,quiet = False &= help "Не писать лог в консоль"
        ,atatime = 30 &= name "e" &= help "Сколько проксей тестить за подход, по дефолту 30"
        ,timeout = 10 &= help "Передышка между подходами, программа не дожидается конца предыдущего подхода, так что если вы выставите таймаут на 0, то все прокси будут запущены одновременно вне зависимости от atatime. По дефолту 10." &= typ "SECONDS"
        ,strBoard = [] &= argPos 0 &= typ "/board/"
        ,thread = 0 &= argPos 1 &= typ "THREADNUM"
        ,input = [] &= argPos 2 &= typ "Файл_с_проксями"
        ,output = "output" &= help "Файл куда писать отчеканные хорошие прокси, по дефолту \"output\"" &= typFile
        ,banned = "banned" &= help "Файл куда писать забаненные прокси, по дефолту \"banned\"" &= typFile
        ,four'o'four = "404" &= explicit &= name "4" &= name "404" &= help "Файл куда писать забаненные по 404 прокси, по дефолту \"404\"" &= typFile
        ,cloudflare_captcha = "cloudflare-captcha" &= name "cc" &= help "Файл куда писать закапченные по клаудфлеру прокси, по дефолту \"cloudflare-captcha\"" &= typFile
        ,cloudflare_ban = "cloudflare-ban" &= name "cb" &= help "Файл куда писать забаненные по клаудфлеру прокси, по дефолту \"cloudflare-ban\"" &= typFile
        ,bad = "bad" &= name "d" &= help "Файл куда писать прокси с которыми не удалось связаться по каким-то причинам" &= typFile
        }
        &= program "blastchecker"
        &= helpArg [explicit, name "h", name "?", name "help", help "Показать вот эту хуйню"]
        &= versionArg [ignore]
        &= summary "Проксичекер для ссача"
        &= help "Формат файла прокси — по прокси на строку, обязательно указывать порт. Файлы banned и bad включают причины бана и эксепшоны http соответственно"

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Testing russian output..."
    putStrLn "Русский текст, например"
    let md = cmdArgsMode impureAnnotatedCmdargsConfig
    ifM (null <$> getArgs)
        (print md)
        (do conf@Config{..} <- cmdArgsRun md
            let board = fromMaybe (error $ "Не смог прочитать \"" ++ strBoard ++ "\" как борду") $
                            readBoard $ strBoard
            ip <- nub . filter (not . null) . lines <$> readFile input
            unless quiet $ print conf
            let eraseFile f = do
                    unless quiet $ putStrLn $ "erasing file " ++ f
                    writeFile f ""
            unless append $ do
                eraseFile output
                eraseFile banned
                eraseFile four'o'four
                eraseFile cloudflare_captcha
                eraseFile cloudflare_ban
                eraseFile bad
            mainloop board conf ip [])

mainloop :: Board -> Config -> [String] -> [(String, MVar Outcome)] -> IO ()
mainloop _ _ [] [] = putStrLn "Ну вот и всё, ребята." >> return ()
mainloop board Config{..} ips mvs = do
    let ape f s = do
            unless quiet $ putStr ("Записываем в файл \"" ++ f ++ "\" :" ++ s ++ "\n")
            appendFile f (s ++ "\n")
    unless quiet $
        putStrLn $ "Опрашиваем прокси... Проксей для опроса: " ++ show (length mvs)
    nmvs <- flip filterM mvs $ \(ip, mv) -> do
        o <- tryTakeMVar mv
        case o of
            Nothing -> return True
            Just a -> do
                case a of
                    InternalError e ->
                        case fromException $ unErrorException e of
                            Just (StatusCodeException st _ _)
                                | statusCode st == 404
                                -> ape four'o'four ip
                            _ -> ape bad $ ip ++ "| failed, exception was: " ++ show e
                    CloudflareBan -> do
                        ape cloudflare_ban ip
                    CloudflareCaptcha -> do
                        ape cloudflare_captcha ip
                    Banned reason -> do
                        ape banned $ ip ++ "| banned, reason was: " ++ show reason
                    UnknownError -> do
                        ape bad $ ip ++ "| Возможно эта прокси не рабочая, и лишь отдает пустые ответы"
                    x -> do
                        unless quiet $ putStrLn $ "got " ++ show x ++ ", assuming that a proxy is good..."
                        ape output ip
                return False
    let (ci, ni) = splitAt atatime ips
    let txt = ">>" ++ show thread ++ "\nОП-хуй, сажаскрыл."
    plusmv <- forM ci $ \ip -> do
        m <- newEmptyMVar
        void $ forkIO $ runBlast $ do
            unless quiet $ liftIO $ putStrLn $ "Запущен тред для " ++ ip
            setTimeout $ Just $ 30 * 1000000 -- default timeout is 10 seconds, but we want to give slow proxies a chance
            maybe (error $ "Couldn't parse as a proxy \"" ++ ip ++ "\"")
                httpSetProxy (readBlastProxy socks ip)
            unless quiet $ liftIO $ putStrLn $ ip ++ ": Поcтим"
            liftIO . putMVar m . fst =<<
                post =<<
                    prepare board (Just thread)
                        (PostData "САЖА" txt Nothing True False False False) "03AHJ_VutW6y0VOt928pITpHtSRO6mM4Vk-iou_VVxKkC5MCxKKU-rSCdQT-yqaGxHg0y-YZNKDD_n_-bUNFSVDB-G_db5J4RbLIvI-ysf8fd2dXj4Xt6bwG0CRLRgmDrc-NmKQBn89GXVTpEZa2iTJF3Hny3F8e5aNw" "reading speed"
                        ("http://2ch.so" ++ renderBoard board ++ "wakaba.pl")
                        [field "akane" ""] ssachLengthLimit
        return (ip, m)
    unless quiet $ putStrLn $ "Передышка: " ++ show timeout ++ " секунд..., Ещё не запущено: " ++ show (length ni) ++ " проксей."
    threadDelay $ timeout * 1000000
    mainloop board Config{..} ni (reverse plusmv ++ nmvs)
















