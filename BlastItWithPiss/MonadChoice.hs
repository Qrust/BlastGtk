{-# OPTIONS_GHC -fno-warn-orphans #-}
module BlastItWithPiss.MonadChoice
    (module Control.Monad.Random
    ,MonadChoice
    ,chooseFromList
    ,mchooseFromList
    ,generateRandomString
    ,generateSymbolString
    ) where
import Import
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import Control.Monad.Random
import System.Random.Shuffle

type MonadChoice a = (MonadRandom a, MonadIO a, Applicative a)

chooseFromList :: MonadChoice m => [a] -> m a
chooseFromList [] = error "chooseFromList supplied with empty list."
chooseFromList l = (l!!) <$> getRandomR (0, length l - 1)

mchooseFromList :: (Monoid a, MonadChoice m) => [a] -> m a
mchooseFromList [] = return mempty
mchooseFromList l = (l!!) <$> getRandomR (0, length l - 1)

{-# INLINE generateRandomString #-}
generateRandomString :: MonadChoice m => (Int, Int) -> (Char, Char) -> m String
generateRandomString lengthBounds charBounds = do
    len <- getRandomR lengthBounds
    take len <$> getRandomRs charBounds

generateSymbolString :: MonadChoice m => Int -> m String
generateSymbolString maxlength = do
    let plength = maxlength `div` 6
    num <- generateRandomString (0, plength) ('0', '9')
    beng <- generateRandomString (0, plength) ('A', 'Z')
    seng <- generateRandomString (0, plength) ('a', 'z')
    brus <- generateRandomString (0, plength) ('А', 'Я')
    srus <- generateRandomString (0, plength) ('а', 'я')
    spc <- generateRandomString (0, plength) (' ', ' ')
    shuffleM (num++beng++seng++brus++srus++spc)

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
