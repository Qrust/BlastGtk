-- stolen from stm-conduit
-- renamed Chan to Queue
-- added Prelude import
-- changed layout in decRefCount

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes,KindSignatures #-}
-- | * Introduction
--
--   Contains a simple source and sink for linking together conduits in
--   in different threads. Usage is so easy, it's best explained with an
--   example:
--
--   We first create a queue for communication...
--
--   > do queue <- atomically $ newTBMQueue 16
--
--   Then we fork a new thread loading a wackton of pictures into memory. The
--   data (pictures, in this case) will be streamed down the queue to whatever
--   is on the other side.
--
--   >    _ <- forkIO . runResourceT $ loadTextures lotsOfPictures $$ sinkTBMQueue queue
--
--   Finally, we connect something to the other end of the queue. In this
--   case, we connect a sink which uploads the textures one by one to the
--   graphics card.
--
--   >    runResourceT $ sourceTBMQueue queue $$ Conduit.mapM_ (liftIO . uploadToGraphicsCard)
--
--   By running the two tasks in parallel, we no longer have to wait for one
--   texture to upload to the graphics card before reading the next one from
--   disk. This avoids the common switching of bottlenecks (such as between the
--   disk and graphics memory) that most loading processes seem to love.
--
--   Control.Concurrent.STM.TMQueue and Control.Concurrent.STM.TBMQueue are
--   re-exported for convenience.
--
--   * Caveats
--
--   It is recommended to use TBMQueue as much as possible, and generally avoid
--   TMQueue usage. TMQueues are unbounded, and if used, the conduit pipeline
--   will no longer use a bounded amount of space. They will essentially leak
--   memory if the writer is faster than the reader.
--
--   Therefore, use bounded queues as much as possible, preferably with a
--   high bound so it will be hit infrequently.
module Data.Conduit.TMQueue ( -- * Bounded Queue Connectors
                             module Control.Concurrent.STM.TBMQueue
                           , sourceTBMQueue
                           , sinkTBMQueue
                           -- * Unbounded Queue Connectors
                           , module Control.Concurrent.STM.TMQueue
                           , sourceTMQueue
                           , sinkTMQueue
                           -- * Parallel Combinators
                           , (>=<)
                           , mergeSources
                           ) where
import Prelude
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.Trans.Resource
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue

import Data.Conduit
import Data.Conduit.Internal

queueSource 
    :: MonadIO m
    => queue                     -- ^ The queue.
    -> (queue -> STM (Maybe a))  -- ^ The 'read' function.
    -> (queue -> STM ())         -- ^ The 'close' function.
    -> Source m a
queueSource ch reader closer = ConduitM src
    where
        src = PipeM pull
        pull = do a <- liftSTM $ reader ch
                  case a of
                    Just x  -> return $ HaveOutput src close x
                    Nothing -> return $ Done ()
        close = liftSTM $ closer ch
{-# INLINE queueSource #-}

queueSink
    :: MonadIO m
    => queue                     -- ^ The queue.
    -> (queue -> a -> STM ())    -- ^ The 'write' function.
    -> (queue -> STM ())         -- ^ The 'close' function.
    -> Sink a m ()
queueSink ch writer closer = ConduitM sink
    where
        sink = NeedInput push close

        push input = PipeM ((liftIO . atomically $ writer ch input) 
                            >> (return $ NeedInput push close))
        close = const . liftSTM $ closer ch
{-# INLINE queueSink #-}

-- | A simple wrapper around a TBMQueue. As data is pushed into the queue, the
--   source will read it and pass it down the conduit pipeline. When the
--   queue is closed, the source will close also.
--
--   If the queue fills up, the pipeline will stall until values are read.
sourceTBMQueue :: MonadIO m => TBMQueue a -> Source m a
sourceTBMQueue ch = queueSource ch readTBMQueue closeTBMQueue
{-# INLINE sourceTBMQueue #-}

-- | A simple wrapper around a TMQueue. As data is pushed into the queue, the
--   source will read it and pass it down the conduit pipeline. When the
--   queue is closed, the source will close also.
sourceTMQueue :: MonadIO m => TMQueue a -> Source m a
sourceTMQueue ch = queueSource ch readTMQueue closeTMQueue
{-# INLINE sourceTMQueue #-}

-- | A simple wrapper around a TBMQueue. As data is pushed into the sink, it
--   will magically begin to appear in the queue. If the queue is full,
--   the sink will block until space frees up. When the sink is closed, the
--   queue will close too.
sinkTBMQueue :: MonadIO m => TBMQueue a -> Sink a m ()
sinkTBMQueue ch = queueSink ch writeTBMQueue closeTBMQueue
{-# INLINE sinkTBMQueue #-}

-- | A simple wrapper around a TMQueue. As data is pushed into this sink, it
--   will magically begin to appear in the queue. When the sink is closed,
--   the queue will close too.
sinkTMQueue :: MonadIO m => TMQueue a -> Sink a m ()
sinkTMQueue ch = queueSink ch writeTMQueue closeTMQueue
{-# INLINE sinkTMQueue #-}

infixl 5 >=<

-- | Modifies a TVar, returning its new value.
modifyTVar'' :: TVar a -> (a -> a) -> STM a
modifyTVar'' tv f = do x <- f <$> readTVar tv
                       writeTVar tv x
                       return x
liftSTM :: forall (m :: * -> *) a. MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

-- | Combines two sources with an unbounded queue, creating a new source
--   which pulls data from a mix of the two sources: whichever produces first.
--
--   The order of the new source's data is undefined, but it will be some
--   combination of the two given sources.
(>=<) :: (MonadIO m, MonadBaseControl IO m)
      => Source (ResourceT m) a
      -> Source (ResourceT m) a
      -> ResourceT m (Source (ResourceT m) a)
sa >=< sb = mergeSources [ sa, sb ] 16
{-# INLINE (>=<) #-}

decRefcount :: TVar Int -> TBMQueue a ->  STM ()
decRefcount tv queue = do
    n <- modifyTVar'' tv (subtract 1)
    when (n == 0) $
        closeTBMQueue queue

-- | Merges a list of sources, putting them all into a bounded queue, and
--   returns a source which can be pulled from to pull from all the given
--   sources in a first-come-first-serve basis.
--
--   The order of the new source's data is undefined, but it will be some
--   combination of the given sources.
mergeSources :: (MonadIO m, MonadBaseControl IO m)
             => [Source (ResourceT m) a] -- ^ The sources to merge.
             -> Int -- ^ The bound of the intermediate queue.
             -> ResourceT m (Source (ResourceT m) a)
mergeSources sx bound = do c <- liftSTM $ newTBMQueue bound
                           refcount <- liftSTM . newTVar $ length sx
                           mapM_ (\s -> resourceForkIO $ s $$ queueSink c writeTBMQueue $ decRefcount refcount) sx
                           return $ sourceTBMQueue c

