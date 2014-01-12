-- |
-- Module      : Control.Concurrent.STM.FinalizerTVar
-- Copyright   : 2013 kudah
-- License     : MIT
-- Maintainer  : kudah <kudahkukarek@gmail.com>
-- Stability   : experimental
-- Portability : stm >= 2.4
--
-- TVar with a finalizer. If you change the value of a 'FinalizerTVar' or it
-- becomes unreachable, a designated finalizer action will be run to clean up
-- any resources associated with the value.

{-# OPTIONS_GHC -Wall #-}
module Control.Concurrent.STM.FinalizerTVar
    (FinalizerTVar

    ,newFinalizerTVarIO

    ,readFinalizerTVar
    ,readFinalizerTVarIO
    ,writeFinalizerTVar
    ) where
import Prelude

import Control.Concurrent
import Control.Concurrent.STM

import qualified Control.Exception as E

import Control.Monad

data FinalizerTVar a
    = FinalizerTVar
        { -- | the value
          tvalue :: (TVar a)
        -- | the finalizer
        , tfinalizer :: (TVar (IO ()))
        -- | exec queue. Actions pushed here will be immediately executed in a
        -- janitor thread
        , texecqueue :: (TQueue (IO ()))
        }

-- | Create a new 'FinalizerTVar' with supplied value and its finalizer.
--
-- Finalizer will be run in a forkIO'd thread if a new value is written to a
-- 'FinalizerTVar' or it's garbage collected
newFinalizerTVarIO
    :: a
    -> IO ()
    -> IO (FinalizerTVar a)
newFinalizerTVarIO value finalIO = do
    value' <- newTVarIO value
    finalizer <- newTVarIO finalIO
    execqueue <- newTQueueIO

    _ <- forkIO $
        (forever $ join $ atomically $ readTQueue execqueue
        ) `E.catch` \E.BlockedIndefinitelyOnSTM ->
            -- exec died, time to clean up the last finalizer
            join $ readTVarIO finalizer

    return $ FinalizerTVar value' finalizer execqueue

-- | Replace old value and finalizer with new ones.
--
-- Finalizer for the old value is run immediately.
writeFinalizerTVar :: FinalizerTVar a -> a -> IO () -> STM ()
writeFinalizerTVar ft value finalIO = do
    oldfinal <- readTVar (tfinalizer ft)

    writeTVar   (tvalue     ft) value
    writeTVar   (tfinalizer ft) finalIO
    writeTQueue (texecqueue ft) oldfinal

-- | Retrieve value stored in a 'FinalizerTVar'
readFinalizerTVar :: FinalizerTVar a -> STM a
readFinalizerTVar ft =
    readTVar (tvalue ft)

-- | Retrieve value stored in a 'FinalizerTVar'
--
-- Simply reads memory, faster than using 'readFinalizerTVar'
readFinalizerTVarIO :: FinalizerTVar a -> IO a
readFinalizerTVarIO ft =
    readTVarIO (tvalue ft)

{-
test :: IO ()
test = do
    do
        ft <- newFinalizerTVarIO (1 :: Int) (putStrLn "1 died")
        atomically $ do
            writeFinalizerTVar 2 (putStrLn "2 died") ft
            writeFinalizerTVar 3 (putStrLn "3 died") ft
            writeFinalizerTVar 4 (putStrLn "4 died") ft
        threadDelay 1000 -- workaround messed up output in ghci
        putStrLn . ("bye, " ++) . show =<< atomically (readFinalizerTVar ft)
    threadDelay 10000000
    -- we lost track of 'ft', "4 died" should be printed before "end"
    putStrLn "end"
-}
