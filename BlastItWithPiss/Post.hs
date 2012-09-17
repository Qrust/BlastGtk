module BlastItWithPiss.Post where
import Import
import BlastItWithPiss.Board
import BlastItWithPiss.Escaping
import BlastItWithPiss.MultipartFormData
import BlastItWithPiss.Image
import BlastItWithPiss.Blast
import Text.HTML.TagSoup
import qualified Text.Show as S
import qualified Codec.Binary.UTF8.Generic as UTF8
import Control.Monad.Trans.Resource

--FIXME
--{-
import Data.CaseInsensitive
import qualified Data.ByteString.Lazy as L
---}

newtype ErrorMessage = Err {unErrorMessage :: String}
    deriving (Eq, Ord)

instance Show ErrorMessage where
    show = unErrorMessage -- show cyrillic as is, instead of escaping it.

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
             | OtherError {errMessage :: ErrorMessage}
             | InternalError {errMessage :: ErrorMessage}
    deriving (Eq, Show, Ord)

message :: Outcome -> String
message = unErrorMessage . errMessage

successOutcome :: Outcome -> Bool
successOutcome Success = True
successOutcome (SuccessLongPost _) = True
successOutcome _ = False

wordfiltered :: [Tag String] -> Bool
wordfiltered =
    isPrefixOf [TagOpen "html" [], TagOpen "body" [], TagOpen "h1" [],TagText "Spam detected."]

haveAnError :: [Tag String] -> Maybe String
haveAnError tags =
    fromTagText . last <$> getInfixOfP
        [(~== TagOpen ("center"::String) [])
        ,(~== TagOpen ("strong"::String) [])
        ,(~== TagOpen ("font"::String) [("size", "5")])
        ,isTagText
        ] tags

outcome :: [Tag String] -> Outcome
outcome tags
    | wordfiltered tags = Wordfilter
    | Just err <- haveAnError tags =
        case () of
            _ | Just reason <- stripPrefix "Ошибка: Доступ к отправке сообщений с этого IP закрыт. Причина: " err
                -> Banned (Err reason)
              | isInfixOf "Флудить нельзя" err
                -> SameMessage
              | isInfixOf "Этот файл уже был загружен" err
                -> SameImage
              | isInfixOf "Обнаружен флуд" err
                -> TooFastPost
              | isInfixOf "Вы уже создали один тред" err
                -> TooFastThread
              | isInfixOf "забыли ввести капчу" err
                -> NeedCaptcha
              | isInfixOf "Неверный код подтверждения" err
                -> WrongCaptcha
              | isInfixOf "заблокирован на сервере ReCaptcha" err
                -> RecaptchaBan
              | isInfixOf "Слишком большое сообщение" err
                -> LongPost
              | isInfixOf "Загружаемый вами тип файла не поддерживается" err
                -> CorruptedImage
              | otherwise
                -> OtherError (Err err)
    | otherwise = Success

-- | Query adaptive captcha state
doWeNeedCaptcha :: String -> Maybe Int -> Blast Bool
doWeNeedCaptcha wakabapl thread = do
    let td = maybe "" show thread
    elem (TagOpen "div" [("id", "recaptcha_widget")]) <$> httpGetStrTags
        (wakabapl ++ "?task=captcha&thread=" ++ td ++ "&dummy=" ++ td)

getChallengeKey :: String -> Blast String
getChallengeKey key = do
    rawjs <- httpGetStr ("http://api.recaptcha.net/challenge?k=" ++ key ++ "&lang=en")
    return $ headNote ("getChallengeKey: Recaptcha changed their JSON formatting, update code: " ++ rawjs) $
        mapMaybe getChallenge $ lines rawjs
  where getChallenge s =
            takeUntil (=='\'') <$> stripPrefix "challenge : \'" (dropWhile isSpace s)

reloadCaptcha :: String -> String -> Blast ()
reloadCaptcha key chKey = void $
    httpGet $ "http://www.google.com/recaptcha/api/reload?c="
                    ++ chKey ++ "&k=" ++ key ++ "&reason=r&type=image&lang=en"

getCaptchaImage :: String -> Blast LByteString
getCaptchaImage chKey =
    httpGet $ "http://www.google.com/recaptcha/api/image?c=" ++ chKey

getCaptcha :: String -> Maybe Int -> String -> String -> Blast (Maybe LByteString)
getCaptcha wakabapl thread key chKey =
    ifM (doWeNeedCaptcha wakabapl thread)
        (do reloadCaptcha key chKey
            Just <$> getCaptchaImage chKey)
        (return Nothing)

data PostData = PostData
        {subject:: String
        ,text   :: String
        ,image  :: !(Maybe Image)
        ,sage   :: !Bool
        ,makewatermark :: !Bool
        }
--FIXME
--{-
instance NFData Outcome

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString

instance NFData L.ByteString where
    rnf r = L.toChunks r `deepseq` ()
#endif

instance NFData a => NFData (CI a) where
    rnf ci = original ci `deepseq` foldedCase ci `deepseq` ()

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
        proxy r `seq` 
        socksProxy r `seq` 
        rawBody r `deepseq` 
        decompress r `deepseq` 
        redirectCount r `deepseq` 
        checkStatus r `deepseq` 
        responseTimeout r `deepseq`
        ()

---}

prepare :: Board -> Maybe Int -> PostData -> String -> String -> String -> [Field] -> Int -> Blast (Request a, Outcome)
prepare board thread PostData{text=unesctext',..} chKey captcha wakabapl otherfields' maxlength = do
    --print =<< getCurrentTime
    let otherfields = filter (not . reservedField) otherfields' 
    let (unesctext, rest) = case splitAt maxlength unesctext' of
                                    (ut, []) -> (ut, [])
                                    (ut, r) -> (ut, r)
    text <- escape maxlength wordfilter unesctext
    let fields =
            [field "parent" (maybe "" show thread)
            ,field "kasumi" (UTF8.fromString subject)
            ,field "shampoo" (UTF8.fromString text)
            ,field "recaptcha_challenge_field" (fromString chKey)
            ] ++
            ([Field
                [("name", "file")
                ,("filename", maybe mempty (UTF8.fromString . filename) image)]
                [(hContentType, maybe defaultMimeType contentType image)]
                (maybe mempty bytes image)]
            ) ++
            ([field "recaptcha_response_field" (UTF8.fromString captcha)
                | not $ null captcha]
            ) ++
            (if sage
                then [field "nabiki" "sage"
                     ,field "sage" "on"]
                else [field "nabiki" mempty]
            ) ++
            ([field "makewatermark" "on" | makewatermark]
            ) ++
            (otherfields)
    boundary <- randomBoundary
    let body = formatMultipart boundary fields
    --putStrLn $ UTF8.toString $ (\(RequestBodyBS a) -> a) $ body
    req' <- parseUrl wakabapl
    let req = req' {method = methodPost
                   ,requestHeaders =
                        [(hContentType, "multipart/form-data; boundary=" <> boundary)
                        ,(hReferer, ssachBoard board)
                        ,(hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                        ,(hAcceptLanguage, "ru,en;q=0.5")
                        ]
                   ,redirectCount = 0
                   ,requestBody = body
                   ,responseTimeout = Nothing
                   }
    --print =<< getCurrentTime
    return $!! (req, if null rest then Success else SuccessLongPost rest)
  where reservedField f = any (`fieldNameIs` f) ["parent"
                                                ,"kasumi"
                                                ,"shampoo"
                                                ,"file"
                                                ,"sage"
                                                ,"nabiki"
                                                ,"recaptcha_challenge_field"
                                                ,"recaptcha_response_field"
                                                ,"makewatermark"]

post :: (Request (ResourceT IO), Outcome) -> Blast Outcome
post (req, success) = do
    catches
        (outcome <$> httpReqStrTags req)
        [Handler $ \ex -> case ex of
            StatusCodeException st heads
                | statusCode st >= 300 && statusCode st < 400
                , lookup "Location" heads
                  >$> maybe False (isInfixOf "res/" . UTF8.toString)
                    -> return success
            unknownex -> return $ InternalError (Err $ show unknownex)
        ,Handler $ \(async :: AsyncException) -> throwIO async
        ,Handler $ \(something :: SomeException) -> return $ InternalError (Err $ show something)
        ]
