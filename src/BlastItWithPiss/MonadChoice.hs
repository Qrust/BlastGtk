{-# OPTIONS_GHC -fno-warn-orphans #-}
module BlastItWithPiss.MonadChoice
    (module Control.Monad.Random

    ,MonadChoice

    ,chooseFromList
    ,chooseFromListMaybe

    ,generateRandomString
    ,generateSymbolString
    ) where
import Import
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import Control.Monad.Random
import System.Random.Shuffle

type MonadChoice a = (MonadRandom a, MonadIO a, MonadBaseControl IO a, Applicative a)

{-# INLINABLE chooseFromList #-}
chooseFromList :: MonadChoice m => [a] -> m a
chooseFromList []  = error "chooseFromList supplied with empty list."
chooseFromList [x] = return x
chooseFromList l   = (l!!) <$> getRandomR (0, length l - 1)

{-# INLINABLE chooseFromListMaybe #-}
chooseFromListMaybe :: MonadChoice m => [a] -> m (Maybe a)
chooseFromListMaybe []  = return Nothing
chooseFromListMaybe [x] = return (Just x)
chooseFromListMaybe l   = Just . (l!!) <$> getRandomR (0, length l - 1)

{-# INLINABLE generateRandomString #-}
generateRandomString :: MonadChoice m => (Int, Int) -> (Char, Char) -> m String
generateRandomString lengthBounds charBounds = do
    len <- getRandomR lengthBounds
    take len <$> getRandomRs charBounds

{-# INLINABLE generateSymbolString #-}
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

instance MonadRandom m => MonadRandom (ResourceT m) where
    {-# INLINE getRandom #-}
    getRandom   = lift getRandom
    {-# INLINE getRandoms #-}
    getRandoms  = lift getRandoms
    {-# INLINE getRandomR #-}
    getRandomR  = lift . getRandomR
    {-# INLINE getRandomRs #-}
    getRandomRs = lift . getRandomRs

instance MonadRandom m => MonadRandom (StateT s m) where
    {-# INLINE getRandom #-}
    getRandom   = lift getRandom
    {-# INLINE getRandoms #-}
    getRandoms  = lift getRandoms
    {-# INLINE getRandomR #-}
    getRandomR  = lift . getRandomR
    {-# INLINE getRandomRs #-}
    getRandomRs = lift . getRandomRs
