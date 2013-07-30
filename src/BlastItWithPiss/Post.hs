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
import BlastItWithPiss.Parsing

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Failure

import Control.Monad.Trans.Resource

data PostData
    = PostData
        {subject       :: !String
        ,text          :: !String
        ,image         :: !(Maybe JunkImage)
        ,video         :: !Text
        ,sage          :: !Bool
        ,makewatermark :: !Bool
        ,escapeInv     :: !Bool
        ,escapeWrd     :: !Bool
        }
  deriving Show

instance NFData PostData where
    rnf (PostData s t i v sg mw ei ew) = rnf (s,t,i,v,sg,mw,ei,ew)

prepare
    :: (MonadChoice m, Failure HttpException m, MonadResource m')
    => Board
    -> Maybe Int
    -> PostData
    -> CAnswer m m'
    -> [Part m m']
    -> Int
    -> ([Part m m'] -> [Part m m'])
    -> m (Request m', Outcome)
prepare
    board
    thread
    PostData{text=unesctext',..}
    (CAnswer _ captchafields)
    otherfields
    maxlength
    postprocess
    = do
    let
      (unesctext, rest) = splitAt maxlength unesctext'

    text <- escapingFunction escapeInv escapeWrd unesctext

    let fields =
          postprocess $
            ([partBS "parent" (maybe "" show thread)
             ,partBS "kasumi" $ T.encodeUtf8 $ T.pack subject
             ,partBS "shampoo" $ T.encodeUtf8 $ T.pack text
             ,partFileRequestBody "file"
                (maybe mempty (filename . fromJunkImage) image)
                -- TODO upload image using conduit
                (RequestBodyLBS $ maybe mempty (bytes . fromJunkImage) image)
             ,partBS "video" $ T.encodeUtf8 video
            ] ++
            (if sage
                then [partBS "nabiki" "sage"
                     ,partBS "sage" "on"]
                else [partBS "nabiki" mempty]
            ) ++
            (if makewatermark
                then [partBS "makewatermark" "on"]
                else []
            ) ++
            captchafields
            )
            `union'`
            otherfields

    rreq <- parseUrl $ ssachPostUrl board thread

    let req' = rreq {
          requestHeaders =
           [(hReferer, ssachThread board thread)
           ]
          ,responseTimeout = Just 30
          ,redirectCount = 0
          ,checkStatus = \_ _ _ -> Nothing
          }

    req <- formDataBody fields req'

    return $!! (req, if null rest then Success else SuccessLongPost rest)
  where
    {-# INLINE escapingFunction #-}
    escapingFunction True True = escape maxlength ssachWordfilter
    escapingFunction True False = escapeExceptWordfilter maxlength
    escapingFunction False True = escapeWordfilter maxlength ssachWordfilter
    escapingFunction False False = return

    {-# INLINE union' #-}
    union' :: [Part m m'] -> [Part m m'] -> [Part m m']
    union' = unionBy ((==) `on` partName)

-- It's left to the Browser module to postprocess the request
post :: (Request (ResourceT IO), Outcome) -> Blast (Outcome, Maybe Html)
post (req, success) = do

    let exc someex =
          return
            ( InternalError $ ErrorException $ toException someex
            , Nothing)

    (do resp <- httpReqStrTags req
        let !st = responseStatus resp
            heads = responseHeaders resp
            ~tags = responseBody resp
            cj = responseCookieJar resp
        case () of
          _ | statusCode st == 303
            , Just loc <- decodeASCII <$> lookup "Location" heads
             -> if loc == "wakaba.html"
              then
                return (PostRejected, Nothing)
              else
                if T.isInfixOf "res/" loc
                  then
                    return (success, Nothing)
                  else
                    exc $ StatusCodeException st heads cj

            |   statusCode st == 502
             || statusCode st == 520
             || statusCode st == 522
             -> return (PostRejected, Nothing)

            | statusCode st == 503
             -> return (Five'o'ThreeError, Nothing)

            | statusCode st == 403
             -> case detectCloudflare tags of
                  Nothing -> return (Four'o'ThreeBan, Nothing)
                  Just o -> return (o, Just tags)

            | statusCode st == 404
                && maybe False (== "NWS_QPLUS_HY") (lookup hServer heads)
             -> return (Four'o'FourBan, Nothing)

            | statusCode st >= 200 && statusCode st <= 300
             -> return (detectOutcome tags, Just tags)

            | otherwise
             -> exc (StatusCodeException st heads cj)
     ) `catches`
        [Handler $ \(async :: AsyncException) ->
            throwIO async
        ,Handler $ \(something :: SomeException) ->
            return (InternalError (ErrorException something), Nothing)
        ]
