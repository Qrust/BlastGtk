module BlastItWithPiss.Post where
import Import
import BlastItWithPiss.Board
import BlastItWithPiss.Escaping
import BlastItWithPiss.MultipartFormData
import BlastItWithPiss.Image
import BlastItWithPiss.Blast
import Text.HTML.TagSoup
import qualified Text.Show as S
import Control.Monad.Trans.Resource



import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST





newtype ErrorMessage = Err {unErrorMessage :: String}
    deriving (Eq, Ord)

instance Show ErrorMessage where
    show = unErrorMessage -- show cyrillic as is, instead of escaping it.

newtype ErrorException = ErrorException {unErrorException :: SomeException}
    deriving (Typeable)

instance Show ErrorException where
    show = show . unErrorException

instance Eq ErrorException where
    (==) _ _ = True

instance Ord ErrorException where
    compare _ _ = EQ

data Outcome = Success
             | SuccessLongPost {rest :: String}
             | Wordfilter
             | Banned {errMessage :: ErrorMessage}
             | SameMessage
             | SameImage
             | TooFastPost
             | TooFastThread
             | NeedCaptcha
             | WrongCaptcha
             | RecaptchaBan
             | LongPost
             | CorruptedImage
             | CloudflareCaptcha
             | CloudflareBan
             | OtherError {errMessage :: ErrorMessage}
             | InternalError {errException :: ErrorException}
             | UnknownError
    deriving (Eq, Show, Ord)

message :: Outcome -> String
message = unErrorMessage . errMessage

successOutcome :: Outcome -> Bool
successOutcome Success = True
successOutcome (SuccessLongPost _) = True
successOutcome _ = False

wordfiltered :: [Tag T.Text] -> Bool
wordfiltered =
    isPrefixOf [TagOpen "html" [], TagOpen "body" [], TagOpen "h1" [],TagText "Spam detected."]

haveAnError :: [Tag T.Text] -> Maybe T.Text
haveAnError tags =
    fromTagText . last <$> getInfixOfP
        [(~== TagOpen ("center"::T.Text) [])
        ,(~== TagOpen ("strong"::T.Text) [])
        ,(~== TagOpen ("font"::T.Text) [("size", "5")])
        ,isTagText
        ] tags

cloudflareCaptcha :: [Tag T.Text] -> Bool
cloudflareCaptcha =
    isInfixOf [TagOpen "title" [], TagText "Attention required!"]

cloudflareBan :: [Tag T.Text] -> Bool
cloudflareBan =
    isInfixOfP [(==TagOpen "title" []), maybe False (T.isPrefixOf "Access Denied") . maybeTagText]

detectOutcome :: [Tag T.Text] -> Outcome
detectOutcome tags
    | wordfiltered tags = Wordfilter
    | Just err <- haveAnError tags =
        case () of
            _ | Just reason <- T.stripPrefix "Ошибка: Доступ к отправке сообщений с этого IP закрыт. Причина: " err
                -> Banned (Err $ T.unpack reason)
              | T.isInfixOf "Флудить нельзя" err
                -> SameMessage
              | T.isInfixOf "Этот файл уже был загружен" err
                -> SameImage
              | T.isInfixOf "Обнаружен флуд" err
                -> TooFastPost
              | T.isInfixOf "Вы уже создали один тред" err
                -> TooFastThread
              | T.isInfixOf "забыли ввести капчу" err
                -> NeedCaptcha
              | T.isInfixOf "Неверный код подтверждения" err
                -> WrongCaptcha
              | T.isInfixOf "заблокирован на сервере ReCaptcha" err
                -> RecaptchaBan
              | T.isInfixOf "Слишком большое сообщение" err
                -> LongPost
              | T.isInfixOf "Загружаемый вами тип файла не поддерживается" err
                -> CorruptedImage
              | otherwise
                -> OtherError (Err $ T.unpack err)
    | otherwise = UnknownError

detectCloudflare :: [Tag T.Text] -> Maybe Outcome
detectCloudflare tags
    | cloudflareCaptcha tags = Just CloudflareCaptcha
    | cloudflareBan tags = Just CloudflareBan
    | otherwise = Nothing

-- | Query adaptive captcha state
doWeNeedCaptcha :: String -> Maybe Int -> Blast Bool
doWeNeedCaptcha wakabapl thread = do
    let td = maybe "" show thread
    elem (TagOpen "div" [("id", "recaptcha_widget")]) <$> httpGetStrTags
        (wakabapl ++ "?task=captcha&thread=" ++ td ++ "&dummy=" ++ td)

getChallengeKey :: String -> Blast String
getChallengeKey key = do
-- TODO use JSON parser(since we'll be using it for config and update manifest anyway)
    rawjsstr <- T.unpack <$> httpGetStr ("http://api.recaptcha.net/challenge?k=" ++ key ++ "&lang=en")
    return $ headNote ("getChallengeKey: Recaptcha changed their JSON formatting, update code: " ++ rawjsstr) $
        mapMaybe getChallenge $ lines rawjsstr
  where getChallenge s =
            takeUntil (=='\'') <$> stripPrefix "challenge : \'" (dropWhile isSpace s)

reloadCaptcha :: String -> String -> Blast ()
reloadCaptcha key chKey = void $
    httpGet $ "http://www.google.com/recaptcha/api/reload?c="
                    ++ chKey ++ "&k=" ++ key ++ "&reason=r&type=image&lang=en"

getCaptchaImage :: String -> Blast LByteString
getCaptchaImage chKey =
    httpGet $ "http://www.google.com/recaptcha/api/image?c=" ++ chKey

ssachGetCaptcha :: String -> Maybe Int -> String -> String -> Blast (Maybe LByteString)
ssachGetCaptcha wakabapl thread key chKey =
    ifM (doWeNeedCaptcha wakabapl thread)
        (do reloadCaptcha key chKey
            Just <$> getCaptchaImage chKey)
        (return Nothing)
--FIXME
--{-
instance NFData Outcome

instance NFData (RequestBody a) where
    rnf (RequestBodyBS b) = b `deepseq` ()
    rnf (RequestBodyLBS b) = b `deepseq` ()
    rnf a = a `seq` ()

instance NFData (Request a) where
    rnf r =
        method r `deepseq` 
        secure r `deepseq` 
        host r `deepseq` 
        port r `deepseq` 
        path r `deepseq` 
        queryString r `deepseq` 
        requestHeaders r `deepseq` 
        requestBody r `deepseq` 
        proxy r `deepseq` 
        socksProxy r `deepseq` 
        rawBody r `deepseq` 
        decompress r `deepseq` 
        redirectCount r `deepseq` 
        checkStatus r `deepseq` 
        responseTimeout r `deepseq`
        ()

---}

data PostData = PostData
        {subject:: String
        ,text   :: String
        ,image  :: !(Maybe Image)
        ,sage   :: !Bool
        ,makewatermark :: !Bool
        }

prepare :: Bool -> Board -> Maybe Int -> PostData -> String -> String -> String -> [Field] -> Int -> Blast (Request a, Outcome)
prepare esc board thread PostData{text=unesctext',..} chKey captcha wakabapl otherfields maxlength = do
    --print =<< liftIO $ getCurrentTime
    let (unesctext, rest) = case splitAt maxlength unesctext' of
                                    (ut, []) -> (ut, [])
                                    (ut, r) -> (ut, r)
    text <- if esc
                then escape maxlength wordfilter unesctext
                else return unesctext
    let fields =
            [field "parent" (maybe "" show thread)
            ,field "kasumi" (T.encodeUtf8 $ T.pack subject)
            ,field "shampoo" (T.encodeUtf8 $ T.pack text)
            ,field "recaptcha_challenge_field" (fromString chKey)
            ] ++
            ([Field
                [("name", "file")
                ,("filename", maybe mempty (ST.encodeUtf8 . ST.pack . filename) image)]
                [(hContentType, maybe defaultMimeType contentType image)]
                (maybe mempty bytes image)]
            ) ++
            ([field "recaptcha_response_field" (T.encodeUtf8 $ T.pack captcha)
                | not $ null captcha]
            ) ++
            (if sage
                then [field "nabiki" "sage"
                     ,field "sage" "on"]
                else [field "nabiki" mempty]
            ) ++
            ([field "makewatermark" "on" | makewatermark]
            )
            `union`
            (otherfields)
    boundary <- randomBoundary
    let body = formatMultipart boundary fields
    {-
    liftIO $ putStrLn $ UTF8.toString $ (\(RequestBodyLBS a) -> a) $ body
    undefined
    -}
    req' <- parseUrl wakabapl
    let req = req' {
                    method = methodPost
                   ,requestHeaders =
                        [(hContentType, "multipart/form-data; boundary=" <> boundary)
                        ,(hReferer, ssachBoard board)
                        ,(hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                        ,(hAcceptLanguage, "ru,en;q=0.5")
                        ]
                   ,requestBody = body

                   ,responseTimeout = Just 30
                   ,redirectCount = 0
                   ,checkStatus = \_ _ -> Nothing
                   }
    --liftIO $ print =<< getCurrentTime
    return $!! (req, if null rest then Success else SuccessLongPost rest)

post :: (Request (ResourceT IO), Outcome) -> Blast (Outcome, Maybe [Tag T.Text])
post (req, success) = do
    catches
        (do Response st _ heads tags <- httpReqStrTags req
            case()of
             _ | (statusCode st >= 300 && statusCode st < 400)
                 && (maybe False (ST.isInfixOf "res/" . ST.decodeASCII) $
                        lookup "Location" heads)
                -> return (success, Nothing)
               | statusCode st == 403
                -> maybe (throwIO $ StatusCodeException st heads Nothing)
                    (return . flip (,) (Just tags)) $ detectCloudflare tags
               | statusCode st >= 200 && statusCode st <= 300
                -> return (detectOutcome tags, Just tags)
               | otherwise
                -> throwIO (StatusCodeException st heads Nothing)
            )
        [Handler $ \(async :: AsyncException) ->
            throwIO async
        ,Handler $ \(something :: SomeException) ->
            return (InternalError (ErrorException something), Nothing)
        ]
