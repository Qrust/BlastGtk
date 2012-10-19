module BlastItWithPiss.Post
    (PostData(..)
    ,doWeNeedCaptcha
    ,getChallengeKey
    ,reloadCaptcha
    ,getCaptchaImage
    ,ssachGetCaptcha

    ,prepare
    ,post
    ) where
import Import
import BlastItWithPiss.Board
import BlastItWithPiss.Escaping
import BlastItWithPiss.MultipartFormData
import BlastItWithPiss.Image
import BlastItWithPiss.Blast
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import BlastItWithPiss.Parsing


data PostData = PostData
        {subject:: String
        ,text   :: String
        ,image  :: !(Maybe Image)
        ,sage   :: !Bool
        ,makewatermark :: !Bool
        ,escapeInv :: !Bool
        ,escapeWrd :: !Bool
        }

-- | Query adaptive captcha state
doWeNeedCaptcha :: Board -> Maybe Int -> String -> Blast Bool
doWeNeedCaptcha board thread usercode = do
    cd <- responseBody <$> httpReqStr
        (fromJust $ parseUrl $ ssach ++ "/makaba/captcha?code=" ++ usercode)
            {requestHeaders = [(hAccept, "text/html, */*; q=0.01")
                              ,("x-requested-with", "XMLHttpRequest")
                              ,(hReferer, ssachThread board thread)]}
    return $ not (T.isInfixOf "OK" cd || T.isInfixOf "VIP" cd)

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

ssachGetCaptcha :: Board -> Maybe Int -> String -> String -> Blast (Maybe LByteString)
ssachGetCaptcha board thread key chKey =
    ifM (doWeNeedCaptcha board thread "")
        (do reloadCaptcha key chKey
            Just <$> getCaptchaImage chKey)
        (return Nothing)

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

prepare :: Board -> Maybe Int -> PostData -> String -> String -> String -> [Field] -> Int -> Blast (Request a, Outcome)
prepare board thread PostData{text=unesctext',..} chKey captcha wakabapl otherfields maxlength = do
    --print =<< liftIO $ getCurrentTime
    let (unesctext, rest) = case splitAt maxlength unesctext' of
                                    (ut, []) -> (ut, [])
                                    (ut, r) -> (ut, r)
    let escapingFunction True True = escape maxlength wordfilter
        escapingFunction True False = escapeExceptWordfilter maxlength
        escapingFunction False True = escapeWordfilter maxlength wordfilter
        escapingFunction False False = return
    text <- escapingFunction escapeInv escapeWrd unesctext
    let fields = (
            [field "parent" (maybe "" show thread)
            ,field "kasumi" (T.encodeUtf8 $ T.pack subject)
            ,field "shampoo" (T.encodeUtf8 $ T.pack text)
            ] ++
            ([Field
                [("name", "file")
                ,("filename", maybe mempty (T.encodeUtf8 . T.pack . filename) image)]
                [(hContentType, maybe defaultMimeType contentType image)]
                (maybe mempty bytes image)]
            ) ++
            (if not $ null captcha
                then
                    [field "recaptcha_challenge_field" (fromString chKey)
                    ,field "recaptcha_response_field" (T.encodeUtf8 $ T.pack captcha)
                    ]
                else []
            ) ++
            (if sage
                then [field "nabiki" "sage"
                     ,field "sage" "on"]
                else [field "nabiki" mempty]
            ) ++
            ([field "makewatermark" "on" | makewatermark]
            ))
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
                        ,(hReferer, ssachThread board thread)
                        ,(hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                        ,(hAcceptLanguage, "ru,en;q=0.5")
                        ]
                   ,requestBody = body

                   ,responseTimeout = Just 30
                   ,redirectCount = 0
                   ,checkStatus = \_ _ -> Nothing
                   }
    --liftIO $ print =<< getCurrentTime
    req `deepseq` return (req, if null rest then Success else SuccessLongPost rest)

post :: (Request (ResourceT IO), Outcome) -> Blast (Outcome, Maybe Html)
post (req, success) = do
    catches
        (do Response st _ heads ~tags <- httpReqStrTags req
            case()of
             _ | (statusCode st >= 300 && statusCode st < 400) &&
                 (maybe False (T.isInfixOf "res/" . T.decodeASCII) $
                        lookup "Location" heads)
                -> return (success, Nothing)
               | statusCode st == 403
                -> maybe (throwIO $ StatusCodeException st heads)
                    (return . flip (,) (Just tags)) $ detectCloudflare tags
               | statusCode st == 404 && (maybe False (=="NWS_QPLUS_HY") $
                    lookup hServer heads)
                -> return (Four'o'FourBan, Nothing)
               | statusCode st >= 200 && statusCode st <= 300
                -> return (detectOutcome tags, Just tags)
               | otherwise
                -> throwIO (StatusCodeException st heads)
            )
        [Handler $ \(async :: AsyncException) ->
            throwIO async
        ,Handler $ \(something :: SomeException) ->
            return (InternalError (ErrorException something), Nothing)
        ]
