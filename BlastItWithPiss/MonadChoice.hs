{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module BlastItWithPiss.MonadChoice
    (module Control.Monad.Random
    ,MonadChoice
    ,chooseFromList
    ) where
import Import
import Control.Monad.Trans.Resource
import Control.Monad.Random

instance MonadIO m => MonadRandom (ResourceT m) where
    getRandom = liftIO getRandom
    getRandoms = liftIO getRandoms
    getRandomR = liftIO . getRandomR
    getRandomRs = liftIO . getRandomRs

class (MonadRandom m, MonadIO m, Applicative m) => MonadChoice m

instance (MonadRandom m, MonadIO m, Applicative m) => MonadChoice m

chooseFromList :: MonadChoice m => [a] -> m a
chooseFromList [] = error "chooseFromList supplied with empty list."
chooseFromList l = (l!!) <$> getRandomR (0, length l - 1)