{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module BlastItWithPiss.Captcha
    (currentSsachCaptchaType

    ,unsafeMakeYandexCaptchaAnswer

    ,ssachRecaptchaKey
    ,cloudflareRecaptchaKey
    ,ssachSolveMediaKey

    ,CAnswer(..)

    ,Captcha(..)

    ,Recaptcha(..)
    ,recaptchaChallengeKey

    ,Yandex(..)

    ,makabaCaptcha
    ) where
import Import
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Board
import BlastItWithPiss.Blast
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Recognition.Antigate

ssachRecaptchaKey :: String
ssachRecaptchaKey = "6LdOEMMSAAAAAIGhmYodlkflEb2C-xgPjyATLnxx"

cloudflareRecaptchaKey :: String
cloudflareRecaptchaKey = "6LeT6gcAAAAAAAZ_yDmTMqPH57dJQZdQcu6VFqog"

ssachSolveMediaKey :: String
ssachSolveMediaKey = "oIzJ06xKCH-H6PKr8OLVMa26G06kK3qh"

type UserCode = String

currentSsachCaptchaType :: Yandex
currentSsachCaptchaType = undefined

data CAnswer m m' = CAnswer
    { cAdaptive :: !Bool -- ^ Adaptive captcha?
    , cFields :: [Part m m']
    }
  deriving Show

-- | Kludge
instance Default (CAnswer m m') where
    def = CAnswer True []

class Captcha a where
    -- | Check if any captcha is needed and return either premade fields or key
    -- needed to solve challenge.
    getNewCaptcha :: (MonadChoice m, MonadResource m') => Board -> Maybe Int -> UserCode -> Blast (Either (CAnswer m m') a)
    -- | If they use systems like recaptcha or solveMedia, then we know their
    -- public key before hand, so we don't have to query makaba to get our challenge.
    unsafeGenNewCaptcha :: Maybe (Blast a)
    unsafeGenNewCaptcha = Nothing
    -- reloadCaptcha :: a -> Blast ()
    getCaptchaImage :: a -> Blast (LByteString, MimeType)
    applyCaptcha :: (MonadChoice m, MonadResource m') => a -> String -> Blast (CAnswer m m')
    getCaptchaConf :: a -> Blast CaptchaConf

newtype Recaptcha = Recaptcha {recaptchaKey :: String}

instance Captcha Recaptcha where
    getNewCaptcha board thread usercode = do
        res <- makabaCaptcha board thread usercode
        if T.isInfixOf "OK" res || T.isInfixOf "VIP" res
          then return $ Left $ CAnswer True []
          else Right . Recaptcha <$> recaptchaChallengeKey ssachRecaptchaKey

    unsafeGenNewCaptcha =
        Just (Recaptcha <$> recaptchaChallengeKey ssachRecaptchaKey)
{-
    reloadCaptcha (Recaptcha chKey) = do
        void $ httpGetLbs $
            "http://www.google.com/recaptcha/api/reload?c=" ++ chKey ++ "&k=" ++
                ssachRecaptchaKey ++ "&reason=r&type=image&lang=en"
-}
    getCaptchaImage (Recaptcha chKey) = do
        res <- httpGetLbs $
            "http://www.google.com/recaptcha/api/image?c=" ++ chKey
        return (res, "image/jpg")

    applyCaptcha (Recaptcha chKey) answer = return $ CAnswer False $
        [partBS "recaptcha_challenge_field" (fromString chKey)
        ,partBS "recaptcha_response_field" (T.encodeUtf8 $ T.pack answer)]

    getCaptchaConf _ = return $ def {phrase = True}

newtype Yandex = Yandex {yandexKey :: String}

instance Captcha Yandex where
    getNewCaptcha board thread usercode = do
        res <- makabaCaptcha board thread usercode
        if T.isInfixOf "OK" res || T.isInfixOf "VIP" res
          then return $ Left $ CAnswer True []
          else
            let !str = T.unpack res
            in return $ Right $ Yandex $ lastNote (yandexerr str) $ lines str
      where yandexerr a = "Yandex captcha: Challenge ID not found in \"" ++ a ++
                            "\". Update code."

    unsafeGenNewCaptcha = Nothing

    getCaptchaImage (Yandex chKey) = do
        res <- httpGetLbs $ "http://i.captcha.yandex.net/image?key=" ++ chKey
        return (res, "image/gif")

    applyCaptcha (Yandex chKey) answer =
        return $ unsafeMakeYandexCaptchaAnswer chKey answer

    getCaptchaConf _ = return $ def {numeric=Just True}

unsafeMakeYandexCaptchaAnswer :: (Monad m, Monad m') => String -> String -> CAnswer m m'
unsafeMakeYandexCaptchaAnswer chKey answer =
    CAnswer False
        [partBS "captcha" (fromString chKey)
        ,partBS "captcha_value_id_06" (fromString answer)
        ]

-- | Query adaptive captcha state
makabaCaptcha :: Board -> Maybe Int -> String -> Blast Text
makabaCaptcha _board _thread usercode = do
    let code = if not $ null usercode then "?usercode=" ++ usercode else []
    responseBody <$> httpReqStr
        (fromJust $ parseUrl $ ssach ++ "/makaba/captcha.fcgi" ++ code)
            {requestHeaders = [(hAccept, "text/html, */*; q=0.01")
                              ,("X-Requested-With", "XMLHttpRequest")
                            -- ,(hReferer, ssachThread board thread)
                              ]}

recaptchaChallengeKey :: String -> Blast String
recaptchaChallengeKey key = do
    rawjsstr <- T.unpack <$> httpGetStr recaptchaUrl
    return $ fromMaybe (error $ fatalErrorMsg ++ ": " ++ rawjsstr) $
            findMap getChallenge $ lines rawjsstr
  where getChallenge s =
            takeUntil (=='\'') <$>
                stripPrefix "challenge : \'" (dropWhile isSpace s)
        fatalErrorMsg =
            "FATAL ERROR: getNewCaptcha: Recaptcha changed their "
            ++ "JSON formatting, update code"
        recaptchaUrl =
            "http://api.recaptcha.net/challenge?k=" ++ key ++ "&lang=en"

{-
getChallengeKey :: String -> Blast String
getChallengeKey key = do
    rawjsstr <- T.unpack <$> httpGetStr ("http://api.recaptcha.net/challenge?k=" ++ key ++ "&lang=en")
    return $ headNote (fatalErrorMsg ++ ": " ++ rawjsstr) $
        mapMaybe getChallenge $ lines rawjsstr
  where getChallenge s =
            takeUntil (=='\'') <$> stripPrefix "challenge : \'" (dropWhile isSpace s)
        fatalErrorMsg = "FATAL ERROR: getChallengeKey: Recaptcha changed their JSON formatting, update code"

reloadCaptcha :: String -> String -> Blast ()
reloadCaptcha key chKey = void $
    httpGet $ "http://www.google.com/recaptcha/api/reload?c="
                    ++ chKey ++ "&k=" ++ key ++ "&reason=r&type=image&lang=en"

getCaptchaImage :: String -> Blast LByteString
getCaptchaImage chKey =
    httpGetLbs $ "http://www.google.com/recaptcha/api/image?c=" ++ chKey

ssachGetCaptcha :: Board -> Maybe Int -> String -> String -> Blast (Maybe LByteString)
ssachGetCaptcha board thread key chKey =
    ifM (doWeNeedCaptcha board thread "")
        (do reloadCaptcha key chKey
            Just <$> getCaptchaImage chKey)
        (return Nothing)
-}

{-

data SolveMedia = SolveMedia
    {solveMediaKey :: String
    ,solveMediaMagic :: String
    ,solveMediaTStamp :: !Int
    ,solveMediaChalStamp :: !Int
    ,solveMediaCallbacks :: !(IORef Int) -- ^ How many times we reloaded captcha
    ,solveMediaFwv :: String
    ,solveMediaChid :: !(IORef String)
    }

instance Captcha SolveMedia where
    getNewCaptcha board thread usercode = do
        res <- makabaCaptcha board thread usercode
        if T.isInfixOf "OK" res || T.isInfixOf "VIP" res
          then return $ Left $ CAnswer True []
          else do
            let ckey = fromMaybe ssachSolveMediaKey $ atMay $ lines res

            chScriptLines <- fmap (lines . T.unpack) $ accept $ httpGetLbs $
                "http://api.solvemedia.com/papi/challenge.script?k=" ++ ckey

            let
              !magic = fromMaybe (magicerr $ unlines chScriptLines) $
                flip findMap chScriptLines $ \x ->
                  takeUntil (=='\'') . tail . dropUntil (=='\'') <$>
                    stripPrefix "magic" (dropWhile isSpace x)

              !chalstamp = fromMaybe (chalstamperr $ unlines chScriptLines) $
                flip findMap chScriptLines $ \x ->
                  readMay . takeUntil (==',') . dropUntil isNumber =<<
                    stripPrefix "chalstamp" (dropWhile isSpace x)

            _puzzle <- accept $ httpGetLbs "http://api.solvemedia.com/papi/_puzzle.js"

            fwid <- take 4 <$> getRandomRs ('a', 'z')
            ctx_bN :: Int <- getRandomR (10, 99)
      where
        accept = withOverrideHeader (hAccept, "*/*")
        magicerr = error . ("SolveMedia: Couldn't read magic from " ++)
        chalstamperr = error . ("SolveMedia: Couldn't read chalstamp from " ++)

    unsafeGenNewCaptcha = Just $

-}
