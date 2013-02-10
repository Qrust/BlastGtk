{-# LANGUAGE OverloadedStrings, NondecreasingIndentation #-}
module Main (main) where
import Import
import Updater.Manifest
import System.Environment
import Network.HTTP.Types
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import System.Console.Readline
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.FilePath
import Codec.Binary.UTF8.Generic (toString)
import System.Cmd
import qualified Paths_blast_it_with_piss as Paths
import Text.ParserCombinators.ReadP
import Crypto.Classes hiding (encode)
import Data.Version
import Control.Concurrent
import Control.Monad.Identity

username :: IsString a => a
username = "exbb2"

reponame :: IsString a => a
reponame = "BlastItWithPiss"

githubDownloadsUrl :: String -> URL
githubDownloadsUrl filename =
    "https://github.com/downloads/" ++ username ++ "/" ++ reponame ++ "/" ++ filename

getToVersion :: String -> Maybe String
getToVersion s' =
    let s = dropExtension s'
    in stripPrefix "BlastItWithPiss-linux-x86-" s <|>
       stripPrefix "BlastItWithPiss-windows-x86-" s

emptyUpdate :: UpdateManifest
emptyUpdate = UpdateManifest
    {version = Paths.version
    ,binaryAndResourcesZipArchives = []
    ,imagePackZipArchives = []
    ,changelog = []
    }

only201 :: Status -> ResponseHeaders -> Maybe SomeException
only201 s e =
    if statusCode s /= 201
        then Just $ toException $ StatusCodeException s e
        else Nothing

deleteDownload :: Text -> Int -> IO ()
deleteDownload pass id = do
    void $ withManager $ http $ applyBasicAuth username (encodeUtf8 pass)
        (fromJust $ parseUrl $
            "https://api.github.com/repos/" ++ username ++ "/" ++ reponame ++ "/downloads/" ++ show id)
                {method=methodDelete}

fromJsonParser :: FromJSON a => LByteString -> (a -> Parser b) -> b
fromJsonParser lbs m =
    (either (\e -> error $ "Couldn't parse lbs: \"" ++ e ++ "\"\nLBS was {\n" ++ toString lbs ++ "\n}")
           id $
           flip parseEither (fromMaybe (error $ "JSONFAIL " ++ toString lbs) $ decode lbs) m)

parseGithubDownloadsPart1Response :: Monad a => LByteString -> ByteString -> FilePath -> LByteString -> (Request a, Int)
parseGithubDownloadsPart1Response lbs boundary arcfilename arcbytes =
    fromJsonParser lbs $ \o -> do
    let corr s3 part1 = partBS s3 <$> o .: part1
    fields <- sequence
        [corr "key" "path"
        ,corr "acl" "acl"
        ,return $ partBS "success_action_status" "201"
        ,corr "Filename" "name"
        ,corr "AWSAccessKeyId" "accesskeyid"
        ,corr "Policy" "policy"
        ,corr "Signature" "signature"
        ,corr "Content-Type" "mime_type"
        ,return $ partFileRequestBody "file" arcfilename $ RequestBodyLBS arcbytes
        ]
    rurl <- fromJust . parseUrl <$> o .: "s3_url"
    id <- o .: "id"
    return $ (formDataBodyPure boundary fields $ rurl
        {method = methodPost
        ,requestHeaders = [(hUserAgent, "http-conduit")]
        ,checkStatus = only201
        }
        , id)

uploadZip :: Text -> String -> Text -> ByteString -> IO ()
uploadZip pass arcfilename desc arcbytes = withManager $ \m -> do
    let req = applyBasicAuth username (encodeUtf8 pass) $
                (fromJust $ parseUrl $
                    "https://api.github.com/repos/" ++ username ++ "/" ++ reponame ++ "/downloads")
                {method=methodPost
                ,requestBody = RequestBodyLBS $ encode $ object
                    ["name" .= T.pack arcfilename
                    ,"size" .= B.length arcbytes
                    ,"description" .= desc
                    ,"content_type" .= ("application/zip" :: Text)
                    ]
                ,checkStatus = only201
                }
    liftIO $ putStrLn "Part1"
    lbs <- responseBody <$> httpLbs req m
    liftIO $ putStrLn "Part2"
    boundary <- liftIO $ webkitBoundary
    let (req,id) = parseGithubDownloadsPart1Response lbs boundary arcfilename (toLBS arcbytes)
    void $ httpLbs req m
    liftIO $ putStrLn "Success."

deleteDownloads :: Text -> Version -> IO ()
deleteDownloads pass v = do
    lbs <- simpleHttp $ "https://api.github.com/repos/" ++ username ++ "/" ++ reponame ++ "/downloads"
    let downs = fromJsonParser lbs $ mapM $ \o -> do
            (,) <$> o .: "name" <*> o .: "id"
    forM_ downs $ \(name, id) ->
        case lastMay =<< readP_to_S parseVersion <$> getToVersion name of
            Just (dv,_) -> when (dv < v) $ do
                putStrLn $ "Deleting download " ++ show (name, id) ++ "\n  Version older than current " ++ showVersion dv ++ " < " ++ showVersion v
                putStrLn $ "Waiting a bit so you can cancel..."
                threadDelay $ 2000000
                deleteDownload pass id
            Nothing -> do
                putStrLn $ "Passing by " ++ show (name, id)

getPassword :: String -> IO Text
getPassword desc = do
    hSetEcho stdin False
    password <- maybe (error "Пароль обязателен") T.pack <$> readline (desc ++ "\n")
    hSetEcho stdin True
    return password

main :: IO ()
main = do
    [rv,la,wa,dogit,delold] <- getArgs
    putStrLn "Updating manifest..."
    let v = fst $ last $ readP_to_S parseVersion $ rv
    let lafilename = takeFileName la
    let wafilename = takeFileName wa
    manifest' <- either
        (\(_::SomeException) -> emptyUpdate)
        (fromMaybe emptyUpdate . decode . toLBS) <$>
            try (B.readFile "UPDATE_MANIFEST")
    chlog <- map (\x -> if x==';' then '\n' else x) .
                fromMaybe (error "Please write changelog") <$>
                    readline "Changes in this version:\n"
    putStrLn "Computing linux archive hash"
    labytes <- B.readFile la
    let lasum = renderMD5 $ hash' labytes
    putStrLn "Computing windows archive hash"
    wabytes <- B.readFile wa
    let wasum = renderMD5 $ hash' wabytes
    putStrLn "Updating manifest"
    let manifest = manifest'{version = v
                            ,binaryAndResourcesZipArchives =
                                 [(Linux, (githubDownloadsUrl lafilename, lasum))
                                 ,(Windows, (githubDownloadsUrl wafilename, wasum))]
                            ,changelog = (v, chlog) : changelog manifest'
                            }
    L.writeFile "UPDATE_MANIFEST" $ encodePretty manifest
    putStrLn "Updated manifest..."
    password <- getPassword "Пароль гитхаба:"
    putStrLn "Прыщи..."
    uploadZip password lafilename "Прыщи" labytes
    putStrLn "Сперма..."
    uploadZip password wafilename "Сперма" wabytes
    putStrLn "Загрузилось наконец."
    when (read dogit) $ do
        putStrLn "git commit"
        print =<< rawSystem "git" ["commit", "-am", chlog]
        putStrLn "git push"
        print =<< rawSystem "git" ["push"]
    when (read delold) $ do
        putStrLn "Deleting old downloads..."
        deleteDownloads password v
