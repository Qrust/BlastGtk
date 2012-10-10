module GtkBlast.IO
    (MonadIO(..)
    ,io
    ,fromIOEM) where
import Prelude (IO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Exception.Lifted

io :: MonadIO m => IO a -> m a
io = liftIO

fromIOEM :: MonadBaseControl IO m => m a -> m a -> m a
fromIOEM v = handle (\(_::IOException) -> v)
