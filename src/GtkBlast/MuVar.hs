{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , TypeSynonymInstances #-}
module GtkBlast.MuVar
    (MuVar(..)
    ,get
    ,set
    ,setr
    ,seti
    ,setir
    ,mod
    ,modi
    ,modM
    ,modiM
    ) where
import Import hiding (mod)
import Graphics.UI.Gtk hiding (get, set)
-- import Control.Concurrent.STM -- never make instances for stm

class MuVar v a | v -> a where
    getIO :: v -> IO a
    setIO :: v -> a -> IO ()

instance MuVar (IORef a) a where
    {-# INLINE getIO #-}
    getIO = readIORef
    {-# INLINE setIO #-}
    setIO = writeIORef

instance MuVar CheckButton Bool where
    {-# INLINE getIO #-}
    getIO = toggleButtonGetActive
    {-# INLINE setIO #-}
    setIO = toggleButtonSetActive

instance MuVar Entry String where
    {-# INLINE getIO #-}
    getIO = entryGetText
    {-# INLINE setIO #-}
    setIO = entrySetText

instance MuVar Expander Bool where
    {-# INLINE getIO #-}
    getIO = expanderGetExpanded
    {-# INLINE setIO #-}
    setIO = expanderSetExpanded

instance MuVar SpinButton Double where
    {-# INLINE getIO #-}
    getIO = spinButtonGetValue
    {-# INLINE setIO #-}
    setIO = spinButtonSetValue

{-# INLINE get #-}
get :: (MonadIO m, MuVar v a) => v -> m a
get v = liftIO (getIO v)

{-# INLINE set #-}
set :: (MonadIO m, MuVar v a) => v -> a -> m ()
set v a = liftIO (setIO v a)

{-# INLINE setr #-}
setr :: (Functor m, MonadIO m, MuVar v a) => v -> a -> m v
setr v a = v <$ set v a

{-# INLINE seti #-}
seti :: (MonadIO m, MuVar v a) => a -> v -> m ()
seti = flip set

{-# INLINE setir #-}
setir :: (Functor m, MonadIO m, MuVar v a) => a -> v -> m v
setir = flip setr

{-# INLINE mod #-}
mod :: (Functor m, MonadIO m, MuVar v a) => v -> (a -> a) -> m ()
mod v f = set v =<< f <$> get v

{-# INLINE modi #-}
modi :: (Functor m, MonadIO m, MuVar v a) => (a -> a) -> v -> m ()
modi = flip mod

{-# INLINE modM #-}
modM :: (MonadIO m, MuVar v a) => v -> (a -> m a) -> m ()
modM v m = set v =<< m =<< get v

{-# INLINE modiM #-}
modiM :: (MonadIO m, MuVar v a) => (a -> m a) -> v -> m ()
modiM = flip modM
