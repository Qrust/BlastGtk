-- |
-- Module      : Control.Concurrent.STM.TLQueue
-- Copyright   : 2013 kudah
-- License     :
-- Maintainer  : kudah <kudahkukarek@gmail.com>
-- Stability   : experimental
-- Portability : stm >= 2.4
--
-- TQueue which keeps track of its length

{-# OPTIONS_GHC -Wall #-}
module Control.Concurrent.STM.TLQueue
    ( TLQueue
    , newTLQueue
    , readTLQueue
    , writeTLQueue
    , lengthTLQueue
    ) where
import Prelude

import Control.Concurrent.STM

data TLQueue a
    = TLQueue
        {tq :: TQueue a
        ,ln :: TVar Int
        }

newTLQueue :: STM (TLQueue a)
newTLQueue = do
    x <- newTQueue
    y <- newTVar 0
    return $ TLQueue x y

readTLQueue :: TLQueue a -> STM a
readTLQueue tlq = do
    modifyTVar' (ln tlq) (subtract 1)
    readTQueue $ tq tlq

writeTLQueue :: TLQueue a -> a -> STM ()
writeTLQueue tlq a = do
    modifyTVar' (ln tlq) (+1)
    writeTQueue (tq tlq) a

lengthTLQueue :: TLQueue a -> STM Int
lengthTLQueue = do
    readTVar . ln
