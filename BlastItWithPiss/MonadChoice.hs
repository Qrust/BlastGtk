{-# OPTIONS_GHC -fno-warn-orphans #-}
module BlastItWithPiss.MonadChoice
    (module Control.Monad.Random
    ,MonadChoice
    ,chooseFromList
    ,mchooseFromList
    ) where
import Import
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import Control.Monad.Random

type MonadChoice a = (MonadRandom a, MonadIO a, Applicative a)

chooseFromList :: MonadChoice m => [a] -> m a
chooseFromList [] = error "chooseFromList supplied with empty list."
chooseFromList l = (l!!) <$> getRandomR (0, length l - 1)

mchooseFromList :: (Monoid a, MonadChoice m) => [a] -> m a
mchooseFromList [] = return mempty
mchooseFromList l = (l!!) <$> getRandomR (0, length l - 1)

instance MonadIO m => MonadRandom (ResourceT m) where
    getRandom = liftIO getRandom
    getRandoms = liftIO getRandoms
    getRandomR = liftIO . getRandomR
    getRandomRs = liftIO . getRandomRs

instance MonadRandom m => MonadRandom (StateT s m) where
    getRandom = lift getRandom
    getRandoms = lift getRandoms
    getRandomR = lift . getRandomR
    getRandomRs = lift . getRandomRs
