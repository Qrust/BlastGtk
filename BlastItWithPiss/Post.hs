module BlastItWithPiss.Post
    (module BlastItWithPiss.Captcha
    ,PostData(..)
    ,prepare
    ,post
    ) where
import Import
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Board
import BlastItWithPiss.Escaping
import BlastItWithPiss.MultipartFormData
import BlastItWithPiss.Image
import BlastItWithPiss.Captcha
import BlastItWithPiss.Blast
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Failure


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

instance NFData PostData where
    rnf (PostData s t i sg mw ei ew) = rnf (s,t,i,sg,mw,ei,ew)

instance NFData (RequestBody a) where
    rnf (RequestBodyBS b) = rnf b
    rnf (RequestBodyLBS b) = rnf b
    rnf (RequestBodyBuilder i b) = i `seq` b `seq` ()
    rnf _ = ()

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

prepare :: (MonadChoice m, Failure HttpException m) => Board -> Maybe Int -> PostData -> String -> String -> String -> [Field] -> Int -> m (Request a, Outcome)
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
            ([field "parent" (maybe "" show thread)
             ,field "kasumi" (T.encodeUtf8 $ T.pack subject)
             ,field "shampoo" (T.encodeUtf8 $ T.pack text)
            ]) ++
            ([Field
                [("name", "file")
                ,("filename", maybe mempty (T.encodeUtf8 . T.pack . filename) image)]
                [(hContentType, maybe defaultMimeType contentType image)]
                (maybe mempty bytes image)
            ]) ++
            (if not $ null captcha
                then
                    [field "recaptcha_challenge_field" (fromString chKey)
                    ,field "recaptcha_response_field" (T.encodeUtf8 $ T.pack captcha)]
                else []
            ) ++
            (if sage
                then [field "nabiki" "sage"
                     ,field "sage" "on"]
                else [field "nabiki" mempty]
            ) ++
            (if makewatermark
                then [field "makewatermark" "on"]
                else []
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
    let final = (req, if null rest then Success else SuccessLongPost rest)
    --liftIO $ print =<< getCurrentTime
    final `deepseq` return final

post :: (Request (ResourceT IO), Outcome) -> Blast (Outcome, Maybe Html)
post (req, success) = do
    let exc som = return (InternalError $ ErrorException $ toException som, Nothing)
    catches
        (do Response st _ heads ~tags <- httpReqStrTags req
            case()of
             _ | statusCode st == 303
               , Just loc <- T.decodeASCII <$> lookup "Location" heads
               -> if loc == "wakaba.html"
                    then return (PostRejected, Nothing)
                    else if T.isInfixOf "res/" loc
                            then return (success, Nothing)
                            else exc (StatusCodeException st heads)
               | statusCode st == 503
                -> return (Five'o'ThreeError, Nothing)
               | statusCode st == 403
                -> maybe (return (Four'o'ThreeBan, Nothing))
                    (return . flip (,) (Just tags)) $ detectCloudflare tags
               | statusCode st == 404 && (maybe False (=="NWS_QPLUS_HY") $
                    lookup hServer heads)
                -> return (Four'o'FourBan, Nothing)
               | statusCode st >= 200 && statusCode st <= 300
                -> return (detectOutcome tags, Just tags)
               | otherwise
                -> exc (StatusCodeException st heads)
            )
        [Handler $ \(async :: AsyncException) ->
            throwIO async
        ,Handler $ \(something :: SomeException) ->
            return (InternalError (ErrorException something), Nothing)
        ]
