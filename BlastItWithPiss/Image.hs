module BlastItWithPiss.Image where
import Import hiding (insert)
import BlastItWithPiss.MonadChoice
import Data.Map (insert)
import Network.Mime
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

mimeMap :: MimeMap
mimeMap = insert "jpe" "image/jpeg" defaultMimeMap

data Image = Image {filename :: !String
                   ,contentType :: !ByteString
                   ,bytes :: !LByteString}

appendJunkB :: MonadChoice m => LByteString -> m LByteString
appendJunkB b = do
    bytecount <- getRandomR (128, 10240)
    L.append b . L.pack . take bytecount <$> getRandomRs (1, 255)

appendJunk :: MonadChoice m => Image -> m Image
appendJunk i = do
    b <- appendJunkB (bytes i)
    return i{bytes=b}

readImageWithoutJunk :: MonadIO m => String -> m Image
readImageWithoutJunk fn = do
    bs <- liftIO $ L.readFile fn
    return Image{filename = fn
                ,contentType = mimeByExt mimeMap defaultMimeType $ fromString fn
                ,bytes = bs}
