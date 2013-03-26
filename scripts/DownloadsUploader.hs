{-# LANGUAGE OverloadedStrings, NondecreasingIndentation #-}
module Main (main) where
import Import
import Updater.Manifest
import qualified Paths_blast_it_with_piss as Paths

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

import Crypto.Classes hiding (encode)

import System.Environment
import System.Cmd

import Control.Concurrent
import Control.Monad.Identity

import Data.Version
import Text.ParserCombinators.ReadP

import System.IO (putStrLn, print)

emptyUpdate :: UpdateManifest
emptyUpdate = UpdateManifest
    {version = Paths.version
    ,binaryAndResourcesZipArchives = []
    ,imagePackZipArchives = []
    ,changelog = []
    }

-- | Shortcut for 'fromJust . parseUrl'
{-# INLINE url #-}
url :: String -> Request m
url = fromJust . parseUrl

username :: IsString a => a
username = "exbb2"

reponame :: IsString a => a
reponame = "BlastItWithPiss"

githubDownloadsUrl :: String -> String
githubDownloadsUrl filename =
    "https://github.com/downloads/" ++
        username ++ "/" ++ reponame ++ "/" ++ filename

getToVersion :: String -> Maybe String
getToVersion s' =
    let s = dropExtension s'
    in stripPrefix "BlastItWithPiss-linux-x86-" s <|>
       stripPrefix "BlastItWithPiss-windows-x86-" s

only201 :: Status -> ResponseHeaders -> CookieJar -> Maybe SomeException
only201 s@Status{statusCode=201} e cj = Nothing
only201 s e cj = Just $ toException $ StatusCodeException s e cj

fromJsonParser :: FromJSON a => LByteString -> (a -> Parser b) -> b
fromJsonParser lbs m =
    case parseEither m (fromMaybe (error $ "JSONFAIL " ++ toString lbs) $ decode lbs) of
      Left e -> error $ "Couldn't parse lbs: \"" ++ e ++ "\"\nLBS was {\n" ++ toString lbs ++ "\n}"
      Right a -> a

parseGithubDownloadsPart1Response
    :: Monad a
    => LByteString
    -> ByteString
    -> FilePath
    -> LByteString
    -> (Request a, Int)
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
    id' <- o .: "id"
    return $
      (formDataBodyPure boundary fields $ rurl
        {method = methodPost
        ,requestHeaders = [(hUserAgent, "http-conduit")]
        ,checkStatus = only201
        }
      ,id')

getPassword :: String -> IO Text
getPassword desc = do
    hSetEcho stdin False
    password <- maybe (error "Пароль обязателен") T.pack <$> readline (desc ++ "\n")
    hSetEcho stdin True
    return password

uploadZip :: Text -> String -> Text -> ByteString -> IO Bool
uploadZip pass arcfilename desc arcbytes = withManager $ \m -> do
    let
      _req = fromJust $ parseUrl $
        "https://api.github.com/repos/" ++ username ++ "/" ++ reponame ++ "/downloads"
      req =
        applyBasicAuth username (encodeUtf8 pass) $ _req
            {method = methodPost
            ,requestBody = RequestBodyLBS $ encode $ object
                ["name" .= T.pack arcfilename
                ,"size" .= B.length arcbytes
                ,"description" .= desc
                ,"content_type" .= ("application/zip" :: Text)
                ]
            ,checkStatus = only201
            }

    _e <- try $ do
        liftIO $ putStrLn "Part1"
        lbs <- responseBody <$> httpLbs req m
        boundary <- liftIO $ webkitBoundary
        let (req,_) = parseGithubDownloadsPart1Response lbs boundary arcfilename (toLBS arcbytes)

        liftIO $ putStrLn "Part2"
        void $ httpLbs req m

    case _e of
      Left (StatusCodeException Status{statusCode=401} _ _) -> do
        return False
      Right _ ->
        return True
      Left e -> throwIO e

uploadZips :: [(String, Text, ByteString)] -> IO ()
uploadZips as = do
    pass <- getPassword prompt
    go as pass
  where
    prompt = "Пароль гитхаба:"
    go [] _ = return ()
    go _retry@((fname, desc, bs):us) pass = do
        putStrLn $ "Uploading " ++ T.unpack desc
        ifM (uploadZip pass fname desc bs)
          (go us pass)
          (go _retry =<< getPassword prompt)

deleteDownload :: Text -> Int -> IO ()
deleteDownload pass id = do
    let _req = url $
            "https://api.github.com/repos/" ++ username
            ++ "/" ++ reponame ++ "/downloads/" ++ show id
        req = applyBasicAuth username (encodeUtf8 pass) $ _req{method="DELETE"}
    void $ withManager $ httpLbs req

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

main :: IO ()
main = do
    [rv,la,wa,dogit,delold] <- getArgs
    putStrLn "Updating manifest..."
    let v = fst $ last $ readP_to_S parseVersion $ rv

    let lafilename = takeFileName la
    let wafilename = takeFileName wa

    manifest' <- do
        x <- try $ B.readFile "UPDATE_MANIFEST"
        case x of
          Left (_::SomeException) -> return emptyUpdate
          Right old -> return $ fromMaybe emptyUpdate $ decode $ toLBS old

    let semicolonToNewline = map (\x -> if x==';' then '\n' else x)

    !chlog <- do
        l <-  readline "Changes in this version:\n"
        return $ maybe (error "changelog is required") semicolonToNewline l

    putStrLn "Computing linux archive hash"
    labytes <- B.readFile la
    let !lasum = renderMD5 $ hash' labytes

    putStrLn "Computing windows archive hash"
    wabytes <- B.readFile wa
    let !wasum = renderMD5 $ hash' wabytes

    putStrLn "Updating manifest"
    let manifest = manifest'
            {version = v
            ,binaryAndResourcesZipArchives =
                [(Linux, (githubDownloadsUrl lafilename, lasum))
                ,(Windows, (githubDownloadsUrl wafilename, wasum))]
            ,changelog = (v, chlog) : changelog manifest'
            }
    L.writeFile "UPDATE_MANIFEST" $ encodePretty manifest
    putStrLn "Updated manifest..."

    uploadZips
        [(lafilename, "Вайпалка для Linux", labytes)
        ,(wafilename, "Вайпалка для Шiндoшs. Версия для прыщей работает гораздо быстрее и лучше", wabytes)
        ]

    putStrLn "Done uploading."

    when (read dogit) $ do
        putStrLn "git commit"
        print =<< rawSystem "git" ["commit", "-am", chlog]
        putStrLn "git push"
        print =<< rawSystem "git" ["push"]

    when (read delold) $ do
        putStrLn "Deleting old downloads..."
        password <- getPassword "Пароль гитхаба для удаления старых загрузок:"
        deleteDownloads password v
