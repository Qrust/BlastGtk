{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}
module GtkBlast.MuVar
    (MuVar(..)
    ,get
    ,set
    ,seti
    ,mod
    ,modi
    ,modM
    ,modiM
    ) where
import Import hiding (mod)
import GtkBlast.IO
import Graphics.UI.Gtk hiding (get, set)
import Control.Concurrent.STM

class MuVar v a | v -> a where
    getIO :: v -> IO a
    setIO :: v -> a -> IO ()

get :: (MonadIO m, MuVar v a) => v -> m a
get v = io (getIO v)

set :: (MonadIO m, MuVar v a) => v -> a -> m ()
set v a = io (setIO v a)

seti :: (MonadIO m, MuVar v a) => a -> v -> m ()
seti = flip set

mod :: (Functor m, MonadIO m, MuVar v a) => v -> (a -> a) -> m ()
mod v f = set v =<< f <$> get v

modi :: (Functor m, MonadIO m, MuVar v a) => (a -> a) -> v -> m ()
modi = flip mod

modM :: (MonadIO m, MuVar v a) => v -> (a -> m a) -> m ()
modM v m = set v =<< m =<< get v

modiM :: (MonadIO m, MuVar v a) => (a -> m a) -> v -> m ()
modiM = flip modM

instance MuVar (IORef a) a where
    getIO = readIORef
    setIO = writeIORef

instance MuVar (TVar a) a where
    getIO = readTVarIO
    setIO v a = atomically $ writeTVar v a

instance MuVar CheckButton Bool where
    getIO = toggleButtonGetActive
    setIO = toggleButtonSetActive

instance MuVar Entry String where
    getIO = entryGetText
    setIO = entrySetText

instance MuVar Expander Bool where
    getIO = expanderGetExpanded
    setIO = expanderSetExpanded
