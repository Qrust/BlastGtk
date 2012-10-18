{-# LANGUAGE OverloadedStrings, NondecreasingIndentation #-}
module Main where
import Import
import Updater.Manifest
import BlastItWithPiss.MultipartFormData
import System.Environment
import Network.HTTP.Types
import Network.HTTP.Conduit
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

username :: IsString a => a
username = "exbb2"

reponame :: IsString a => a
reponame = "BlastItWithPiss"

githubDownloadsUrl :: String -> URL
githubDownloadsUrl filename =
    "https://github.com/downloads/" ++ username ++ "/" ++ reponame ++ "/" ++ filename

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

parseGithubDownloadsPart1Response :: LByteString -> ByteString -> FilePath -> LByteString -> (Request a, Int)
parseGithubDownloadsPart1Response lbs boundary arcfilename arcbytes =
    either (\e -> error $ "Couldn't parse lbs: \"" ++ e ++ "\"\nLBS was {\n" ++ toString lbs ++ "\n}")
    id $ flip parseEither (fromMaybe (error $ "JSONFAIL " ++ toString lbs) $ decode lbs) $ \o -> do
    let corr s3 part1 = field s3 <$> o .: part1
    fields <- sequence
        [corr "key" "path"
        ,corr "acl" "acl"
        ,return $ field "success_action_status" "201"
        ,corr "Filename" "name"
        ,corr "AWSAccessKeyId" "accesskeyid"
        ,corr "Policy" "policy"
        ,corr "Signature" "signature"
        ,corr "Content-Type" "mime_type"
        ,return $ Field
               [("name", "file") ,("filename", encodeUtf8 $ T.pack arcfilename)]
               [(hContentType, "application/zip")]
               arcbytes
        ]
    rurl <- fromJust . parseUrl <$> o .: "s3_url"
    id <- o .: "id"
    return $ (rurl
        {method = methodPost
        ,requestHeaders = [(hContentType, "multipart/form-data; boundary=" <> boundary)
                          ,(hUserAgent, "http-conduit")]
        ,requestBody = formatMultipart boundary fields
        ,checkStatus = only201
        }
        , id)

uploadZip :: Int -> Text -> String -> ByteString -> Text -> IO ()
uploadZip t pass arcfilename arcbytes desc = withManager $ \m -> do
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
    boundary <- liftIO $ randomBoundary
    let (req,id) = parseGithubDownloadsPart1Response lbs boundary arcfilename (toLBS arcbytes)
    e <- try $ http req m
    case e of
        Left (a::SomeException) -> do
            -- Oh well, if that's the only way...
            -- Randomly fails with
            -- hPutBuf: resource vanished (Broken pipe)
            -- or hPutBuf: resource vanished (Connection reset by peer)
            liftIO $ putStrLn $ "Got exception: " ++ show a ++ ", restarting..."
            if t < 10
                then do
                    void $ flip http m $ applyBasicAuth username (encodeUtf8 pass)
                        (fromJust $ parseUrl $
                            "https://api.github.com/repos/" ++ username ++ "/" ++ reponame ++ "/downloads/" ++ show id)
                                {method=methodDelete}
                    liftIO $ uploadZip (t+1) pass arcfilename arcbytes desc
                else throwIO a
        Right _ -> do
            liftIO $ putStrLn "Success."

main :: IO ()
main = do
    [rv,la,wa,dogit] <- getArgs
    putStrLn "Updating manifest..."
    let v = fst $ last $ readP_to_S parseVersion $ rv
    let lafilename = takeFileName la
    let wafilename = takeFileName wa
    manifest' <- either
        (\(_::SomeException) -> emptyUpdate)
        (fromMaybe emptyUpdate . decode . toLBS) <$>
            try (B.readFile "UPDATE_MANIFEST")
    hSetEcho stdin True
    chlog <- fromMaybe (error "Please write changelog") <$> readline "Changes in this version:\n"
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
    hSetEcho stdin False
    password <- maybe (error "Пароль обязателен") T.pack <$> readline "Пароль гитхаба:\n"
    putStrLn "Прыщи..."
    uploadZip 0 password lafilename labytes "Прыщи"
    putStrLn "Сперма..."
    uploadZip 0 password wafilename wabytes "Сперма"
    putStrLn "Загрузилось наконец."
    when (read dogit) $ do
        putStrLn "git commit"
        print =<< rawSystem "git" ["commit", "-am", chlog]
        putStrLn "git push"
        print =<< rawSystem "git" ["push"]