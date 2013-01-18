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
import BlastItWithPiss.Image
import BlastItWithPiss.Captcha
import BlastItWithPiss.Blast
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Failure
import Data.CaseInsensitive


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

instance NFData a => NFData (CI a) where
    rnf ci = original ci `deepseq` foldedCase ci `deepseq` ()

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

prepare :: (MonadChoice m, Failure HttpException m, MonadResource m') => Board -> Maybe Int -> PostData -> CAnswer m m' -> [Part m m'] -> Int -> m (Request m', Outcome)
prepare board thread PostData{text=unesctext',..} (CAnswer _ captchafields) otherfields maxlength = do
    let (unesctext, rest) =
          case splitAt maxlength unesctext' of
            (ut, []) -> (ut, [])
            (ut, r) -> (ut, r)
    text <- escapingFunction escapeInv escapeWrd unesctext
    let fields = (
            ([partBS "parent" (maybe "" show thread)
             ,partBS "kasumi" (T.encodeUtf8 $ T.pack subject)
             ,partBS "shampoo" (T.encodeUtf8 $ T.pack text)
             ,partFileRequestBody "file"
                (maybe mempty filename image)
                -- HACK upload image source
                (RequestBodyLBS $ maybe mempty bytes image)
            ]) ++
            (if sage
                then [partBS "nabiki" "sage"
                     ,partBS "sage" "on"]
                else [partBS "nabiki" mempty]
            ) ++
            (if makewatermark
                then [partBS "makewatermark" "on"]
                else []
            ) ++
            (captchafields
            ))
            `union'`
            (otherfields)
    rreq <- parseUrl $ ssachPostUrl board thread
    let req' = rreq {
          requestHeaders =
           [(hReferer, ssachThread board thread)
           ,(hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
           ,(hAcceptLanguage, "ru,en;q=0.5")]   
          ,responseTimeout = Just 30
          ,redirectCount = 0
          ,checkStatus = \_ _ -> Nothing
          }
    req <- formDataBody fields req'
    let final = (req, if null rest then Success else SuccessLongPost rest)
    final `deepseq` return final
  where
    escapingFunction True True = escape maxlength wordfilter
    escapingFunction True False = escapeExceptWordfilter maxlength
    escapingFunction False True = escapeWordfilter maxlength wordfilter
    escapingFunction False False = return

    union' :: [Part m m'] -> [Part m m'] -> [Part m m']
    union' = unionBy ((==) `on` partName)

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
