module BlastItWithPiss.Image where
import Import hiding (insert)
import Data.Map (insert)
import Network.Mime
import qualified Data.ByteString as B
import Control.Monad.Random

mimeMap :: MimeMap
mimeMap = insert "jpe" "image/jpeg" defaultMimeMap

data Image = Image {filename :: !String
                   ,contentType :: !ByteString
                   ,bytes :: !ByteString}

appendJunkB :: ByteString -> IO ByteString
appendJunkB b = do
    bytecount <- getRandomR (128, 10240)
    B.append b . B.pack . take bytecount <$> getRandomRs (1, 255)

appendJunk :: Image -> IO Image
appendJunk i = do
    b <- appendJunkB (bytes i)
    return i{bytes=b}

readImageWithoutJunk :: String -> IO Image
readImageWithoutJunk fn = do
    bs <- B.readFile fn
    return Image{filename = fn
                ,contentType = mimeByExt mimeMap defaultMimeType $ fromString fn
                ,bytes = bs}
