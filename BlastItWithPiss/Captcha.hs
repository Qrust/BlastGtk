module BlastItWithPiss.Captcha where
import Import
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Board
import BlastItWithPiss.Blast
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Failure
{-
type UserCode = String

class ChallengeKey a where
    -- | Check if any captcha is needed and return new challenge key.
    -- If no captcha is needed return Nothing, otherwise 'throw' up.
    getChallengeKey :: Board -> Maybe Int -> UserCode -> Blast (Maybe a)
    -- | 
    reloadCaptcha :: a -> Blast ()
-}
-- | Query adaptive captcha state
doWeNeedCaptcha :: Board -> Maybe Int -> String -> Blast Bool
doWeNeedCaptcha board thread usercode = do
    let code = if not $ null usercode then "?code=" ++ usercode else []
    cd <- responseBody <$> httpReqStr
        (fromJust $ parseUrl $ ssach ++ "/makaba/captcha.fcgi" ++ code)
            {requestHeaders = [(hAccept, "text/html, */*; q=0.01")
                              ,("X-Requested-With", "XMLHttpRequest")
                              ,(hReferer, ssachThread board thread)]}
    return $ not (T.isInfixOf "OK" cd || T.isInfixOf "VIP" cd)

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
