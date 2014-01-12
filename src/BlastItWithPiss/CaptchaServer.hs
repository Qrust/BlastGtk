-- |
-- Module      : BlastItWithPiss.CaptchaServer
-- Copyright   : 2013 kudah
-- License     : GPL-3
-- Maintainer  : kudah <kudahkukarek@gmail.com>
-- Stability   : experimental
-- Portability : ghc-only
--
-- 1. решаем по капче для каждого агента
-- 2. меряем скорость
-- 3. постоянно обновляем скорость
-- 4. правим лимит капч в складе соответствии со скоростью
--    (solvingSpeed / timeoutSeconds) * numProxies
-- 5. Лимит не может быть < numProxies.
-- 6. Если решенных капч меньше лимита — запускать больше тредов с решением,
--    если больше — ждать.

{-# OPTIONS -Wall #-}
module BlastItWithPiss.CaptchaServer
    ( primitiveCaptchaServer
    , presolvingCaptchaServer

    , CaptchaCache
    , newCaptchaCache

    , CaptchaKeysStore
    , newCaptchaKeysStore
    ) where
import Import

import BlastItWithPiss.Types
import BlastItWithPiss.Captcha
import BlastItWithPiss.Image
import BlastItWithPiss.Blast
import BlastItWithPiss.Board
import BlastItWithPiss.MonadChoice

import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Concurrent.STM.TLQueue

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader

-- | Old captcha solver with a minor difference: it will use whatever captchas
--  are stored in the 'CaptchaCache' first, before solving anything by itself,
--  so that you can switch freely between presolving and primitive captcha
--  solvers without fear that your previously solved captcha will be wasted.
--
-- WARNING:
--  All the quirks of the old solver are intact, e.g. it won't bypass cloudflare
--
-- NOTE FIXME TODO HACK WARNING: finalization of the old captcha server is done
-- AFTER this captcha server is constructed. If the old captcha server sets
-- startSignal to False, AFTER this server is constructed, then startSignal will
-- never be set to True, and the wipe won't start. Thankly, captcha servers and
-- GtkBlast currently only set startsignal to True; it's set to False only
-- on start and end of the wipe, captcha servers are restarted afterwards.
primitiveCaptchaServer
    :: (MonadChoice m, MonadResource m')
    => CaptchaCache m m'
    -> TVar Bool
    -> IO (CaptchaServer m m')
primitiveCaptchaServer tlq start = do
    atomically $ writeTVar start True

    return $ CaptchaServer captchaServer
  where
    captchaServer thread = do
     BlastLogData { bBoard } <- ask
     fix $ \recurse -> do
      mx <- liftIO $ atomically $ Just <$> readTLQueue tlq <|> pure Nothing
      case mx of
       Just x -> return $ Just x
       Nothing -> do
        blastLog "Fetching challenge"

        mbbytes <- blast $ getNewCaptcha bBoard thread ""
        case mbbytes of
          Left f -> do
            blastLog $ "Got presolved captcha " ++ show f
                ++ ". Either server said \"OK\" or we're \"VIP\"."
            return (Just (CAWR f (\_ -> return ())))

          Right (chKey :: CurrentSsachCaptchaType) -> do
            blastLog "Downloading captcha"

            (bytes, ct) <- blast $ getCaptchaImage chKey
            cconf <- blast $ getCaptchaConf chKey
            fname <- mkImageFileName ct

            blastLog "Got captcha image, sending captcha mvar"

            m <- newEmptyMVar
            blastCaptcha $
              CaptchaRequest
                {captchaType  = CaptchaPosting
                ,captchaBytes = bytes
                ,captchaSend  = (putMVar m $!!)
                ,captchaConf  = cconf
                ,captchaFilename = fname
                }
            blastLog "blocking on captcha mvar"
            answer <- takeMVar m

            blastLog $ "got captcha mvar, answer is... " ++ show answer
            case answer of
              Answer string report -> do
                f <- blast $ applyCaptcha chKey string
                return $ Just $ CAWR f $ liftIO . report
              ReloadCaptcha -> recurse
              AbortCaptcha -> return Nothing -- no reason to keep this

data Mean a
    = Mean
        {getMean    :: a
        ,meanWeight :: a
        }

addMean :: (Fractional a, Ord a) => a -> Mean a -> Mean a
addMean x (Mean m n) =
    Mean (m + ((x - m) / n')) n'
  where
    n' = n + 1

type CaptchaCache m m'
    = TLQueue (CaptchaAnswerWithReport m m')

{-# INLINABLE newCaptchaCache #-}
newCaptchaCache :: MonadIO m => m (CaptchaCache m0 m1)
newCaptchaCache = liftIO $ atomically newTLQueue

type CaptchaKeysStore m m'
    -- yandex captchas timeout within an hour or so.
    = TLQueue (IO (Maybe (CaptchaAnswerWithReport m m')))

{-# INLINABLE newCaptchaKeysStore #-}
newCaptchaKeysStore :: MonadIO m => m (CaptchaKeysStore m0 m1)
newCaptchaKeysStore = liftIO $ atomically newTLQueue

-- | Presolves captcha to sustain maximum posting speed.
--  Stores solved captcha in the supplied 'CaptchaCache'.
--
-- WARNING:
--  Solver threads created by the server wont't get killed in the finalizer!
--
-- WARNING:
--  Better not keep it running while nothing happens.
--      (Will spawn more solvers when unneeded?)
--  Recreate captcha server each time you restart wipe.
presolvingCaptchaServer
    :: (MonadChoice m, MonadResource m')
    => CaptchaCache m m'
    -> CaptchaKeysStore m m'
    -- | 'writeLog'
    -> (Text -> IO ())
    -- | [(board, (activeAgents, getCurrentPostTimeout)]
    -> [(Board, TVar Int, STM NominalDiffTime)]
    -- | Switch to activate wipe.
    --
    -- Will flip when CaptchaCache gets filled up with enough captcha to
    -- sustain maximum posting speed.
    -> TVar Bool
    -> IO (CaptchaServer m m', IO (), STM PresolverState)
presolvingCaptchaServer tlq ckstore logIO boardsWithData start = do

    aLog "[presolve] Starting presolvingCaptchaServer"

    solversOnline <- newTVarIO 0

    maxLimit <- newTVarIO $ Mean 0 0

    let accessPresolverState = do
          presolverStored   <- lengthTLQueue tlq
          presolverEnRoute  <- readTVar solversOnline
          presolverMaxLimit <- getMean <$> readTVar maxLimit
          presolverKeysIn   <- lengthTLQueue ckstore

          return PresolverState{..}

    deadswitch <- newTVarIO False
    tid <- forkIO $ go solversOnline maxLimit accessPresolverState
        `finally` atomically (writeTVar deadswitch True)

    return
        ( CaptchaServer $ \mthread -> do
          blastLogControl $ \runInIO -> do

            -- while we wait for captcha, grab some captcha keys.
            -- we flip a switch instead of throwing an exception
            -- so that at least one captcha key is acquired every time
            liftIO $ bracket (do
                dswitch <- newTVarIO False

                _ <- forkIO $ runInIO $
                  (fix $ \recurse -> do
                    timeToDie <- liftIO $ readTVarIO dswitch
                    started <- liftIO $ readTVarIO start

                    if timeToDie && started
                      then return ()
                      else do
                        (do ck <- getCaptchaKey mthread accessPresolverState
                            liftIO $ atomically $ writeTLQueue ckstore ck
                         ) `catch` (\(e::HttpException) -> do
                            blastLog $ "[presolve] http exception: " ++ show e
                            case e of
                              StatusCodeException Status{statusCode = 503} _ _
                                -> do blastLog $
                                        "[presolve] 503. Waiting 5 seconds."
                                      liftIO $ threadDelay (5 & millions)
                              StatusCodeException Status{statusCode = 403} _ _
                                -> do blastLog $
                                        "[presolve] 403. Waiting 20 seconds."
                                      liftIO $ threadDelay (20 & millions)
                              _ -> do blastLog $
                                        "[presolve] other. Waiting 2 seconds."
                                      liftIO $ threadDelay (2 & millions)
                            )
                        recurse
                  ) `catch` \(e::SomeException) ->
                    blastLog $ "[presolve] killed. exception was: " ++ show e

                return dswitch
                )
                (\dswitch -> atomically $ writeTVar dswitch True)
                (\_ -> atomically $ do
                    (Just <$> readTLQueue tlq)
                <|> (Nothing <$ (check =<< readTVar deadswitch)))
        ,do killThread tid

            aLog "[presolve] shutting DOWN"
        , accessPresolverState
        )
  where
    getCaptchaKey
        :: (MonadChoice m, MonadResource m')
        => Maybe Int
        -> STM PresolverState
        -> BlastLog (IO (Maybe (CaptchaAnswerWithReport m m')))
    getCaptchaKey mthread accessPresolverState = do
        BlastLogData { bBoard, bCaptchaOut } <- ask

        blastLog "[presolve] Fetching challenge"

        mbbytes <- blast $ getNewCaptcha bBoard mthread ""
        case mbbytes of
          Left f -> do
            blastLog $ "[presolve] Got presolved captcha " ++ show f
                ++ ". Either server said \"OK\" or we're \"VIP\"."
            return $
                return $ Just $ CAWR f $ \_ -> return ()

          Right (chKey :: CurrentSsachCaptchaType) -> do
            blastLog "[presolve] Downloading captcha"

            (bytes, ct) <- blast $ getCaptchaImage chKey
            cconf <- blast $ getCaptchaConf chKey
            fname <- mkImageFileName ct

            blastLog "[presolve] Got captcha image"

            blastLogControl $ \liftToIO -> return $ liftToIO $ do
                m <- newEmptyMVar
                liftIO $ bCaptchaOut PresolverCaptcha $
                  CaptchaRequest
                    {captchaType  = CaptchaPosting
                    ,captchaBytes = bytes
                    ,captchaSend  = (putMVar m $!!)
                    ,captchaConf  = cconf
                    ,captchaFilename = fname
                    }
                blastLog "[presolve] blocking on captcha mvar"
                answer <- takeMVar m

                blastLog $
                    "[presolve] got captcha mvar, answer is... " ++ show answer
                case answer of
                  Answer string report -> do
                    f <- blast $ applyCaptcha chKey string
                    return $ Just $ CAWR f $ liftIO . report
                  ReloadCaptcha ->
                    liftIO =<< getCaptchaKey mthread accessPresolverState
                  AbortCaptcha ->
                    return Nothing

    -- | num agents + average post timeout
    getBoardData :: STM (Int, NominalDiffTime)
    getBoardData = second getMean <$> foldM aux (0, Mean 0 0) boardsWithData
      where
        aux :: (Int, Mean NominalDiffTime)
            -> (Board, TVar Int, STM NominalDiffTime)
            -> STM (Int, Mean NominalDiffTime)
        aux (!agents, !ptimeout) (_, active, getPostTimeout) = do
            a <- readTVar active
            t <- getPostTimeout
            return
                ( agents + a
                , foldl' (flip addMean) ptimeout (replicate a t) )

    go :: TVar Int -> TVar (Mean Rational) -> STM PresolverState -> IO ()
    go solversOnline tmaxLimit accessPresolverState =
      forever $ do
        md <- atomically (do
            p@PresolverState{..} <- do
                p <- accessPresolverState
                (allAgents, _) <- getBoardData

                when (allAgents <= 0) retry

                if presolverMaxLimit p < fromIntegral allAgents
                  then do
                    let nagents = realToFrac allAgents
                    writeTVar tmaxLimit (Mean nagents 0)
                    return p{presolverMaxLimit = nagents}
                  else
                    return p

            let rgot = fromIntegral presolverStored
                     + fromIntegral presolverEnRoute

            if rgot < presolverMaxLimit
              then do
                let need = ceiling $ presolverMaxLimit - rgot
                return $ Just (need, p)
              else do
                started <- readTVar start

                -- we've reached hard limit, time to start wipe
                if not started && presolverEnRoute == 0
                  then do
                    writeTVar start True
                    return Nothing
                  else retry
            )

        whenJust md $ \(need, ps) -> do

          aLog $
              "[presolve] Launching threads to solve " ++ show need
           ++ " captchas while there are " ++ show (presolverStored ps)
           ++ " captchas stored and " ++ show (presolverEnRoute ps)
           ++ " captchas currently in solving."
           ++ " Current limit is "
           ++ show (realToFrac (presolverMaxLimit ps) :: Double)
           ++ ". There are " ++ show (presolverKeysIn ps)
           ++ " captcha keys left."

          replicateM_ need $ forkIO $ bracket_
            (atomically $ modifyTVar' solversOnline (\x -> x + 1))
            (atomically $ modifyTVar' solversOnline (\x -> x - 1))
            $ handle (\(ex::SomeException) ->
            aLog $ "[presolve] solver thread died due to uncaught exception: "
             ++ show ex) $ do

            before <- getCurrentTime
            -- x <- _retrieveAndSolve_'presolve' accessPresolverState
            x <- join $ atomically $ readTLQueue ckstore
            after <- getCurrentTime

            let timeTook = diffUTCTime after before

            -- We don't discard refusals
            -- (we should probably sum time for failed
            -- captchas with the next succesful captcha)
            whenJust x $ atomically . writeTLQueue tlq

            atomically $ do
              (allAgents, averagePostTimeout) <- getBoardData

              modifyTVar' tmaxLimit $ \oldMean ->
                let
                  nagents :: Fractional a => a
                  nagents = realToFrac allAgents

                  -- limit weight to 1.5 num agents
                  lmean =
                    if meanWeight oldMean >= nagents * 1.5
                      then oldMean{meanWeight = nagents * 1.5 - 1}
                      else oldMean

                  nmean =
                    let rounds = if averagePostTimeout /= 0
                          then timeTook / averagePostTimeout
                          else 0.000001
                    in addMean
                      (realToFrac $ rounds * nagents)
                      lmean
                in
                if getMean nmean < nagents
                  then
                    -- make sure there at least as many captchas as the agents
                    nmean{getMean = nagents}
                  else
                    nmean

    {-# INLINE aLog #-}
    aLog :: MonadIO m => Text -> m ()
    aLog = liftIO . logIO
