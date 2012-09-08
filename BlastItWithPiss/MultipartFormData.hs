module BlastItWithPiss.MultipartFormData where
import Import hiding (concat)
import Network.HTTP.Types
import Network.HTTP.Conduit
import Control.Monad.Random
import Data.CaseInsensitive (original)
import Data.ByteString (concat)
--import Codec.Binary.UTF8.Generic (toString, fromString)

data Field = Field {fieldAttrs :: [(ByteString, ByteString)]
                   ,fieldHeaders :: [Header]
                   ,fieldBody :: !ByteString
                   }
    deriving (Eq, Show)

instance Ord Field where
    compare x y | Just a <- lookup "name" $ fieldAttrs x
                , Just b <- lookup "name" $ fieldAttrs y
                    = compare a b
                | otherwise
                    = mempty
                

field :: ByteString -> ByteString -> Field
field n v = Field [("name", n)] [] v

fieldNameIs :: ByteString -> Field -> Bool
fieldNameIs n f = maybe False (==n) $ lookup "name" $ fieldAttrs f

renderField :: ByteString -> Field -> ByteString
renderField boundary (Field params headers body) =
    "--" <> boundary <> "\r\n"
    <> "Content-Disposition: form-data" <> concat (map renderParam params) <> "\r\n"
    <> renderHeaders headers <> "\r\n"
    <> body <> "\r\n"
  where renderParam (n, v) = "; " <> n <> "=\"" <> v <> "\""
        renderHeader (header, val) = original header <> ": " <> val
        renderHeaders [] = mempty
        renderHeaders heads = concat (map renderHeader heads) <> "\r\n"

randomBoundary :: IO ByteString
randomBoundary = do
    dashcount <- getRandomR (9, 50)
    charcount <- getRandomR (30, 50)
    fromString . (replicate dashcount '-' <>)
               . take charcount <$> getRandomRs ('0', 'Z') --wireshark complains that only alphanumeric characters are allowed in boundary, it works however.

formatMultipart :: ByteString -> [Field] -> RequestBody a
formatMultipart boundary fields =
    RequestBodyBS $ concat (map (renderField boundary) fields)
                <> "--" <> boundary <> "--\r\n" --finalizer
