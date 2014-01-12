-- | Hack on a hack whips a hack with a hack. Gross.
module BlastItWithPiss
    (ShSettings(..)
    ,MuSettings(..)
    ,ProxySettings

    ,SageMode(..)
    ,allModes

    ,CaptchaType(..)
    ,CaptchaAnswer(..)
    ,CaptchaOrigin(..)
    ,renderCaptchaOrigin

    ,OriginStamp(oProxy
                ,oBoard
                ,oMode
                ,oThread)

    ,renderCompactStamp
    ,renderFullStamp

    ,CaptchaRequest(..)

    ,Message(..)
    ,OutMessage(..)

    ,TempGenType
    ,mkFullGen
    ,mkIgnoreGen
    ,mkConstGen

    ,defMuS
    ,defPrS
    ,entryPoint

    -- added in 1.2
    ,CaptchaServer(..)
    ,PresolverState(..)
    ,module BlastItWithPiss.CaptchaServer

    -- FIXME
    ,Board(..)
    ,BlastProxy
    ,Blast
    ,Manager
    ,renderBoard

    -- FIXME
    ,TempBlastPastaChannel(..)
    ,sortSsachBoardsByPopularity
    ) where
import Import

import BlastItWithPiss.CaptchaServer
import BlastItWithPiss.Blast
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.Post
import BlastItWithPiss.Types

import Control.Concurrent.STM.FinalizerTVar

-- import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Concurrent.Lifted
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Resource

import Data.Time.Clock.POSIX

import Text.HTML.TagSoup (Tag, renderTags)

-- TVar (IntMap (TMVar Page)) ? TVar (IntMap (TMVar (TMVar Page)))

-- agent can also fail to get a page, same thing as blastCloudflare's lock really.

-- If got no page in five seconds, attempt to fetch on your own.
--  use registerDelay to set timeout
--  OR simply wait for current worker to timeout

-- if no key in IntMap?

-- periodically redownload pages, use max one agent (how?)
--  Simply lock the cache, send one proxy to renew it, make others use old version

-- use proxies with best response time and history of error abscence

-- Agents should not override expiration-thread's modifications. (TMVar IntMap?)
--  Wait, we're already atomic

-- board threads? [!?]

-- You should be able to return captcha to captcha fund if agent failed to use it

{-

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

mockupGet :: TVar (IntMap (TMVar (UTCTime, Page))) -> Int -> IO Page
mockupGet tpageCache pid = do
    ttimeup <- registerDelay (5 `millions`)
    r1 <- atomically $ do
        pageCache <- readTVar tpageCache
        case IntMap.lookup pid pageCache of
          Just waitPage ->
            orElse
              (Right . snd <$> readTMVar waitPage)
              (Left waitPage <$ check <$> readTVar ttimeup)
          Nothing -> do
            tmv <- newEmptyTMVar
            writeTVar tpageCache (IntMap.insert pid tmv pageCache)
            return $ Left tmv
    case r1 of
      Right a ->
        return a
      Left tmv ->
        withAsync (forever getPage) $ \as ->
          atomically $ do
            readTMVar tmv
              <|> do
                utcpg <- waitSTM as
                putTMVar tmv utcpg
                return $ snd utcpg

mockupExpire :: TVar (IntMap (TMVar (UTCTime, Page))) -> IO ()
mockupExpire tpageCache = do
    undefined

-}

-- HACK abortWithOutcome
data AbortOutcome = AbortOutcome !Outcome
    deriving (Show, Typeable)

instance Exception AbortOutcome

abortWithOutcome :: Outcome -> BlastLog a
abortWithOutcome o = do
    blastLog $ "Aborting with outcome: " ++ show o
    blastOut $ OutcomeMessage o
    throwIO  $ AbortOutcome   o
-- /HACK

{-# INLINE readTVarIO #-}
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO

blastPostTimeout :: BlastLog POSIXTime
blastPostTimeout = do
    BlastLogData {
      bBoard = board
    , bShS = ShSettings
        { tposttimeout }
    , bMuS = MuSettings
        { mposttimeout }
    } <- ask

    (fmap realToFrac <$> readTVarIO mposttimeout) >>= fromMaybeM (
        (fmap realToFrac <$> readTVarIO tposttimeout) >>= fromMaybeM (
            return $ ssachPostTimeout board)
        )

blastThreadTimeout :: BlastLog POSIXTime
blastThreadTimeout = do
    BlastLogData {
      bBoard = board
    , bShS = ShSettings
        { tthreadtimeout }
    , bMuS = MuSettings
        { mthreadtimeout }
    } <- ask

    (fmap realToFrac <$> readTVarIO mthreadtimeout) >>= fromMaybeM (
        (fmap realToFrac <$> readTVarIO tthreadtimeout) >>= fromMaybeM (
            return $ ssachThreadTimeout board)
        )

-- FIXME Code duplication with "post" (checking cloudflare)
-- FIXME Code duplication with "primitiveCaptchaServer" (fetching captcha)
blastCloudflare
    :: Request (ResourceT IO)
    -> BlastLog (Response [Tag Text])
blastCloudflare req' = do

    blastLog "Fetching page for cloudflare inspection"

    let
      let_SCC_blastCloudflare = blast $
        httpReqStrTags
          req'
          {checkStatus =
            \a@Status{statusCode} b c
                -> if statusCode == 403 || statusCode == 404 -- 404 for 404ban
                then Nothing
                else (checkStatus def) a b c
          }

    pg <- let_SCC_blastCloudflare

    blastLog "Inspecting"
    blastCloudflare' pg

  where

    blastCloudflare' rsp
      | statusCode == 404 && (maybe False (== "NWS_QPLUS_HY") $
                                lookup hServer $ responseHeaders rsp) = do
        abortWithOutcome Four'o'FourBan -- HACK HACK HACK
      | statusCode == 403 || statusCode == 404 = do
        if cloudflareBan (responseBody rsp)
          then
            abortWithOutcome CloudflareBan -- HACK HACK HACK
          else if cloudflareCaptcha (responseBody rsp)
              then do
                blastLog "Encountered cloudflare challenge"
                cloudflareChallenge rsp
              else throwIO $ StatusCodeException
                    (responseStatus rsp)
                    (responseHeaders rsp)
                    (responseCookieJar rsp)
      | otherwise = do
        blastLog "blastCloudflare: No cloudflare spotted..."
        return rsp
      where
        Status{statusCode} = responseStatus rsp

    cloudflareChallenge rsp = do
        pSharedCookies <- asks $ pSharedCookies . bPrS

        blastLog "Waiting for cloudflare cookies"
        join (modifyMVar pSharedCookies $ \mcookies ->
            case mcookies of
              Nothing -> do
                mres <- solveCloudflareCaptcha rsp

                case mres of
                  Just cj ->
                    return (Just cj, blastCloudflare req')
                  Nothing ->
                    return (Nothing, cloudflareChallenge rsp)
              Just cookies -> do
                blastLog "Got cloudflare cookies"

                old <- blast httpGetCookieJar
                if cookies == old
                  then do
                    blastLog "Same cookies don't work"
                    mres <- solveCloudflareCaptcha rsp

                    case mres of
                      Just cj ->
                        return (Just cj, blastCloudflare req')
                      Nothing ->
                        return (Nothing, cloudflareChallenge rsp)
                  else do
                    blast $ httpSetCookieJar cookies

                    return (Just cookies, blastCloudflare req'))

    solveCloudflareCaptcha rsp =
        (do mcookies <- reallySolveCloudflareCaptcha rsp
            case mcookies of
              Just cookies -> do
                let !cookieCount = length (destroyCookieJar cookies)
                blastLog $ "Cloudflare cookie count: " ++ show cookieCount
                return (Just cookies)
              Nothing -> do
                return Nothing
        ) `catches`
            [Handler $ \(e::AsyncException) -> do
                blastLog "Async exception aborted cloudflare challenge solving"
                throwIO e
            ,Handler $ \(e::SomeException) -> do
                blastLog $
                    "Exception aborted cloudflare challenge solving"
                 ++ ", exception was: " ++ show e
                throwIO e
            ]

    reallySolveCloudflareCaptcha rsp = do
        blastLog "locked cloudflare captcha"

        let !parsedId =
                fromJustNote
                    ("No id field in cloudflare captcha, fill issue at "
                    ++ "https://github.com/exbb2/BlastItWithPiss/issues")
                $ cloudflareIdCaptcha $ responseBody rsp

        chKey <- blast $ recaptchaChallengeKey cloudflareRecaptchaKey

        (bytes, ct) <- blast $ getCaptchaImage $ Recaptcha chKey
        fname <- mkImageFileName ct

        cconf <- blast $ getCaptchaConf $ Recaptcha chKey

        captchaMVar <- newEmptyMVar
        blastCaptcha $
            CaptchaRequest
                {captchaType = CaptchaCloudflare
                ,captchaBytes = bytes
                ,captchaSend = (putMVar captchaMVar $!)
                ,captchaConf = cconf
                ,captchaFilename = fname
                }
        answer <- takeMVar captchaMVar

        case answer of
          Answer s reportBad -> do
            let
              rq = (unsafeParseUrl "http://2ch.hk/cdn-cgi/l/chk_captcha")
                {queryString = renderSimpleQuery True
                    [("recaptcha_challenge_field", fromString chKey)
                    ,("recaptcha_response_field", fromString s)
                    ,("message", "")
                    ,("id", encodeUtf8 parsedId)
                    ]
                ,checkStatus =
                    \a@Status{statusCode} b c
                        -> if statusCode == 403
                          then Nothing
                          else (checkStatus def) a b c
                }

            blastLog "blastCloudflare: finished working on captcha"
            res <- blast $ httpReqStrTags rq

            if statusCode (responseStatus res) == 403
                && cloudflareCaptcha (responseBody res)
              then do
                blastLog "Cloudflare captcha failed, retrying"
                liftIO . reportBad =<< genOriginStamp
                reallySolveCloudflareCaptcha res
              else do
                blastLog "Got through cloudflare captcha."
                cj <- blast httpGetCookieJar
                return $ Just cj
          ReloadCaptcha ->
            reallySolveCloudflareCaptcha rsp
          AbortCaptcha ->
            return Nothing

blastPost
    :: [Part Blast (ResourceT IO)]
    -> Mode
    -> Maybe Int
    -> PostData
    -> BlastLog ()
blastPost otherfields mode thread postdata = do

    BlastLogData
     { bBoard = board
     , bShS = ShSettings
        { tcaptchaserver
        , tfluctuation
        , tstartsignal
        }
    } <- ask

    BlastLogState
     { bsAdaptivityIn
     , bsOptimisticLastPostTime
     , bsPessimisticLastPostTime
     } <- lift get

    mcap <- if not bsAdaptivityIn || mode == CreateNew
      then do
        blastLog "querying captcha"
        captchaServer <- liftIO $ readFinalizerTVarIO tcaptchaserver
        getCaptchaAnswer captchaServer thread
      else do
        blastLog "skipping captcha"
        return $ Just $ CAWR def (const $ return ())
    case mcap of
        Nothing ->
            blastLog "Refused to solve captcha, aborting post"
        Just (CAWR captcha reportbad) ->
          (\x -> fix x 0) $ \retryPost !(retries :: Int) -> do

            p <- blast $ prepare board thread postdata captcha
                            otherfields ssachLengthLimit id

            posttimeout   <- blastPostTimeout
            threadtimeout <- blastThreadTimeout
            blastLog $ "Post timeout: " ++ show posttimeout
            blastLog $ "Thread timeout: " ++ show threadtimeout

            mfluctuation <- readTVarIO tfluctuation
            blastLog $ "Post time fluctuation: " ++ show mfluctuation

            canDefinitelyPost <-
                flip fmap (liftIO getPOSIXTime) $ \now ->
                    now - bsPessimisticLastPostTime >= posttimeout

            -- implying that we get CreateNew only when we definitely can post
            unless (mode == CreateNew || canDefinitelyPost) $ do
                !fluctuation <- flip (maybe $ return 0) mfluctuation $ \f -> do
                    lowerHalf <- chooseFromList [True, True, True, False]
                    r <- realToFrac <$> getRandomR
                        ( if lowerHalf then 0 else f/2
                        , if lowerHalf then f/2 else f )
                    blastLog $ "Sleep added by fluctuation: " ++ show r
                    return r

                let !orientedLastPostTime =
                        if bsAdaptivityIn
                         || (bsPessimisticLastPostTime - bsOptimisticLastPostTime
                            > 2) -- slow proxy
                          then bsOptimisticLastPostTime
                          else bsPessimisticLastPostTime

                now <- liftIO getPOSIXTime
                let slptime =
                        (orientedLastPostTime + posttimeout)
                        - now + fluctuation

                blastLog $ "sleeping " ++ show slptime ++
                    " seconds before post. FIXME using threadDelay for sleeping"
                    ++ ", instead of a more precise timer"
                threadDelay $ toMicroseconds slptime

            whenM (not <$> readTVarIO tstartsignal) $ do
                blastLog $
                    "Right before first post, got my captcha, still waiting for"
                 ++ " start wipe signal..."
                liftIO $ STM.atomically $ STM.check =<< STM.readTVar tstartsignal
                blastLog "Got signal, ENGAGING..."

            blastLog "posting"

            beforePost <- liftIO getPOSIXTime -- optimistic
            (out, _) <- blast $ post p
            afterPost <- liftIO getPOSIXTime -- pessimistic

            blastOut (OutcomeMessage out)

            when (successOutcome out) $ do
                if cAdaptive captcha
                  then setAdaptive True
                  else setAdaptive False
                if mode == CreateNew
                  then do
                    setLastThreadTime afterPost
                  else do
                    setPessimisticLastPostTime afterPost
                    setOptimisticLastPostTime beforePost
                blastLog "post succeded"

            case out of
                Success ->
                    return ()
                SuccessLongPost rest ->
                    when (mode /= CreateNew) $ do
                        blastPost otherfields mode
                            thread postdata{text=rest, image=Nothing}
                Banned bnd -> do
                    blastLog $ "Banned, reason given: " ++ show bnd
                    return ()
                TooFastThread -> do
                    let m = threadtimeout / 4
                    blastLog $ "TooFastThread, retrying in "
                        ++ show (m/60) ++ " minutes"
                    setLastThreadTime $ beforePost - (threadtimeout - m)
                    return ()
                TooFastPost -> do
                    if retries >= 5
                      then do
                        blastLog "TooFastPost, retried five times, aborting."
                        return ()
                      else do
                        blastLog "TooFastPost, retrying in 0.5 seconds"
                        setOptimisticLastPostTime $
                            beforePost - (posttimeout - 0.5)
                        setPessimisticLastPostTime $
                            beforePost - (posttimeout - 0.5)
                        retryPost $ retries + 1
                PostRejected -> do
                    if retries >= 3
                      then do
                        blastLog
                            "PostRejected, retried thrice already, aborting."
                        return ()
                      else do
                        blastLog "PostRejected, retrying immediately..."
                        retryPost $ retries + 1
                o | o == NeedCaptcha || o == WrongCaptcha -> do
                    blastLog $ show o ++ ", requerying"

                    blast . reportbad =<< genOriginStamp

                    blastPost otherfields mode thread postdata
                  |   o == Wordfilter
                   || o == ThreadDoesNotExist
                   || o == SameImage || o == CorruptedImage
                   || o == SameMessage || o == LongPost || o == EmptyPost
                   -> do
                    blastLog $ "post failed with " ++ show o
                    return ()
                -- Internal error, 503, etc
                  | otherwise -> do
                    if retries >= 3
                      then do
                        blastLog $
                            show o ++ ", retried thrice already, aborting."
                        return ()
                      else do
                        blastLog "post failed, retrying in 0.2 seconds"
                        setOptimisticLastPostTime $
                            beforePost - (posttimeout - 0.2)
                        setPessimisticLastPostTime $
                            beforePost - (posttimeout - 0.2)
                        retryPost $ retries + 1

blastPostData :: Mode -> Maybe Page -> Maybe Int -> BlastLog PostData
blastPostData mode mpastapage thread = do
    BlastLogData {
      bShS = ShSettings
        { tpastagen
        , timagegen
        , tvideogen

        , tuseimages
        , tappendjunkimages

        , tsagemode

        , tmakewatermark
        }
    } <- ask

    blastLog "Choosing pasta..."

    _getThread <- blastLogControl $ \runInIO -> return $ runInIO . getThread

    let {-# INLINE runGen #-}
        runGen :: TempGenType a -> BlastLog a
        runGen g = evalGen g _getThread mpastapage thread

    TBPC
     { escinv
     , escwrd
     , pasta
     } <- do
        pastagen <- readTVarIO tpastagen
        runGen pastagen
    blastLog $
        "chose pasta, escaping invisibles " ++ show escinv ++
        ", escaping wordfilter " ++ show escwrd ++ ": \"" ++
        fromString pasta ++ "\""

    sagemode <- readTVarIO tsagemode
    let sage = fromSageMode sagemode mode
    blastLog $ "Sage mode: " ++ show sagemode ++ ", Sage: " ++ show sage

    blastLog "Choosing image"
    cleanImage <- do
        enableImages <- readTVarIO tuseimages
        if (enableImages && not sage) || null pasta || obligatoryImageMode mode
          then do
            -- eval imageGen
            imagegen <- readTVarIO timagegen
            Just <$> runGen imagegen
          else
            return Nothing

    junkingEnabled <- readTVarIO $ tappendjunkimages
    junkImage <-
        case cleanImage of
          Nothing -> do
            return Nothing
          Just i -> Just <$> do
            if junkingEnabled
              then
                appendJunk i
              else
                return $ JunkImage i

    case junkImage of
      Nothing -> blastLog "chose no image"
      Just i -> blastLog $
        "chose image \"" ++ fromString (filename $ fromJunkImage i)
        ++ "\", junk: " ++ show junkingEnabled

    blastLog $ "Choosing video"
    video <- runGen =<< readTVarIO tvideogen
    blastLog $ "Chose video: " ++ video

    watermark <- readTVarIO tmakewatermark
    blastLog $ "Watermark: " ++ show watermark

    let final = PostData
                {subject = ""
                ,text = pasta
                ,image = junkImage
                ,video = video
                ,sage = fromSageMode sagemode mode
                ,makewatermark = watermark
                ,escapeInv = escinv
                ,escapeWrd = escwrd}
    final `deepseq` return final

newtype Couldn'tParseBoardPage = Couldn'tParseBoardPage ErrorMessage
  deriving (Typeable, Show)

instance Exception Couldn'tParseBoardPage

getPage :: Int -> BlastLog Page
getPage p = do
    board <- asks bBoard

    blastLog $ "getPage: going to page " ++ show p

    tags <- responseBody <$> blastCloudflare (unsafeParseUrl (ssachPage board p))
    let !pg = parsePage board tags
    if null (threads pg)
      then throwIO (Couldn'tParseBoardPage $ Err $ renderTags tags)
      else return pg

getThread :: Int -> BlastLog Thread
getThread i = do
    board <- asks bBoard

    blastLog $ "getThread: Going into " ++ ssachThread board (Just i)
        ++ " thread for pasta"

    headNote "PastaHead fail, no posts parsed" . fst . parseThreads
        . responseBody <$>
        blastCloudflare (unsafeParseUrl $ ssachThread board (Just i))

blastLoop :: BlastLog ()
blastLoop =
    forever $ do

    recResetMode
    recResetThread

    BlastLogData {
      bBoard = board
    , bOtherFields
    , bShS = ShSettings
        { tallowedmodes }
    , bMuS = MuSettings
        { mmode
        , mthread }
    } <- ask

    BlastLogState
        { bsLastThreadTime
        } <- lift get

    now <- liftIO getPOSIXTime
    threadtimeout <- blastThreadTimeout
    let canmakethread = now - bsLastThreadTime >= threadtimeout

    mp0 <- do
        threadIsSet <- isJust <$> readTVarIO mthread
        if threadIsSet
          then
            return Nothing
          else Just <$> do
            blastLog "mp0: Getting first page."
            getPage 0

    case mp0 of
      Nothing ->
        blastLog "Thread chosen, ommitting page parsing"
      Just p0 ->
        blastLog $
            "page params:\n"
         ++ "page id: " ++ show (pageId p0) ++ "\n"
         ++ "lastpage id: " ++ show (lastpage p0) ++ "\n"
         ++ "speed: " ++ show (speed p0) ++ "\n"
         ++ "threads: " ++ show (length $ threads p0) ++ "\n"
         ++ "max replies: " ++ show (maximum $ map postcount $ threads p0)

    allowedModes <- readTVarIO tallowedmodes
    let !lstModes = S.toAscList allowedModes
    !mode <- readTVarIO mmode >>=
        maybe
        ((case mp0 of
          Nothing -> do
            blastLog $ "No page, throwing a dice for mode..."
                ++ ", allowed modes: " ++ show lstModes
            chooseFromListMaybe lstModes
          Just p0 -> do
            blastLog $ "Choosing mode..."
                ++ " canmakethread: " ++ show canmakethread
                ++ ", allowed modes: " ++ show lstModes
            chooseMode board canmakethread p0 allowedModes
         ) <&> fromMaybe (error $
                "Неприменимо ни одно поведение из " ++ show lstModes
             ++ "!!" ++ case lstModes of
                -- wait
                [CreateNew] ->
                    "\nРазрешено только создавать треды, но не прошло 30 минут "
                 ++ "с прошлого создания треда."
                [ShitupSticky] ->
                    "\nРазрешено только засирать прикрепленный тред, но ни "
                 ++ "один тред не прикреплен или все прикрепленные треды "
                 ++ "закрыты!"
                _ -> "")
        )
        (\m -> do
            blastLog $ "Got mmode " ++ show m
            return m)
    recMode mode
    blastLog $ "chose mode " ++ show mode

    (thread, mpastapage) <-
        readTVarIO mthread >>= maybe
            (do blastLog "Choosing thread..."
                second Just <$>
                    chooseThread
                        board
                        mode
                        getPage
                        (fromMaybe
                            (error "Page is Nothing while thread not specified")
                            mp0)
            )
            (\t -> do
                blastLog $ "Got mthread " ++ show t
                return (Just t, Nothing))
    recThread thread
    blastLog $ "chose thread " ++ show thread

    !postdata <- blastPostData mode mpastapage thread

    blastPost bOtherFields mode thread postdata

-- | Start wipe with parameters.
entryPoint
    :: Manager
    -> BlastProxy
    -> Board
    -> ShSettings
    -> MuSettings
    -> ProxySettings
    -> (OutMessage -> IO ())
    -> (CaptchaOrigin -> CaptchaRequest -> IO ())
    -> IO ()
entryPoint manager proxy board shS muS prS output coutput =
    bracket_
      (STM.atomically $ STM.modifyTVar (_mMutAgentCount muS) (\x -> x + 1))
      (STM.atomically $ STM.modifyTVar (_mMutAgentCount muS) (\x -> x - 1))
      $
    runBlastNew manager proxy (pUserAgent prS) $
    runBlastLog BlastLogData
        { bProxy = proxy
        , bBoard = board
        , bShS   = shS
        , bMuS   = muS
        , bPrS   = prS
        , bOut   = output
        , bCaptchaOut = coutput
        , bOtherFields = ssachLastRecordedFields board
        }
      $ do
        blastLog "Entry point"

        mask_ $ do
            cj' <- tryTakeMVar (pSharedCookies prS)
            case cj' of
              Just mcj -> do
                putMVar (pSharedCookies prS) mcj
                whenJust mcj $ blast . httpSetCookieJar
              _ -> return ()

        start
  where
    start = flip catches hands $ blastLoop
    hands = [Handler $ \(a::AsyncException) -> do
                blastLog $ "Got async " ++ show a
                throwIO a
            ,Handler $ \(_::AbortOutcome) -> do
                blastLog "HACK abortOutcome"
                -- wait what? {- start -} --HACK abortOutcome
            {-,Handler $ \(a::HttpException) -> do
                blastLog $ "Got http exception, restarting. Exception was: "
                        ++ show a
                start -- Dunno what to do except restart.-}
            ,Handler $ \(a::SomeException) -> do
                blastLog $ "Terminated by exception " ++ show a
                blastOut $ OutcomeMessage $ InternalError $ ErrorException a
            ]

sortSsachBoardsByPopularity
    :: (Text -> IO ())
    -> Maybe (Either (Board, Text) (Board, Int) -> IO ())
    -> [Board]
    -> IO (M.Map Board Int, [Board])
sortSsachBoardsByPopularity logIO mcallback boards = do
    ua <- newUserAgent
    res <- bracket (newManager def) closeManager $ \m ->
      runBlastNew m NoProxy ua $ do
        maybeb <- forM boards $ \b -> do
            liftIO $ logIO $ "Processing " ++ renderBoard b

            rsp :: Either HttpException Html <-
                try $ httpGetStrTags (ssachPage b 0)

            let spd = case rsp of
                    Left x -> Left (show x)
                    Right a ->
                        maybe (Left "Не удалось распарсить скорость") Right $
                            parseSpeed a

            liftIO $ spd & either (\_ -> return ()) (\(s::Int) ->
                logIO $ renderBoard b ++ ": Speed: " ++ show s)

            whenJust mcallback $ \callbackIO ->
                liftIO $ callbackIO (case spd of
                  Left e -> Left (b, e)
                  Right s -> Right (b, s))

            return (b, either (const Nothing) Just spd)

        let (got, failed) = partition (isJust . snd) maybeb
            sorted = reverse $ sortBy (compare `on` fromJust . snd) got

        return
            ( M.fromList $ map (second fromJust) sorted
            , fst $ unzip $ failed)
    liftIO $ logIO "sortSsachBoardsByPopularity: Finished."
    return res
