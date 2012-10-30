module BlastItWithPiss.MultipartFormData
    (Field(..)
    ,field
    ,fieldNameIs

    ,renderField
    ,formatMultipart

    ,randomBoundary
    ) where
import Import hiding (concat)
import BlastItWithPiss.MonadChoice
import Network.HTTP.Types
import Network.HTTP.Conduit
import Data.CaseInsensitive (CI, foldedCase, original)
import qualified Data.ByteString.Lazy as L
--import Codec.Binary.UTF8.Generic (toString, fromString)

-- TODO Blaze.Builder?

data Field = Field {fieldAttrs :: [(ByteString, ByteString)]
                   ,fieldHeaders :: [Header]
                   ,fieldBody :: !LByteString
                   }
    deriving (Show)

instance Eq Field where
    (==) x y | Just a <- lookup "name" $ fieldAttrs x
             , Just b <- lookup "name" $ fieldAttrs y
                = a == b
             | otherwise = False

instance Ord Field where
    compare x y | Just a <- lookup "name" $ fieldAttrs x
                , Just b <- lookup "name" $ fieldAttrs y
                    = compare a b
                | otherwise
                    = mempty

instance NFData a => NFData (CI a) where
    rnf ci = original ci `deepseq` foldedCase ci `deepseq` ()

instance NFData Field where
    rnf Field{..} = rnf (fieldAttrs, fieldHeaders, fieldBody)

randomBoundary :: MonadChoice m => m ByteString
randomBoundary = do
    dashcount <- getRandomR (9, 50)
    charcount <- getRandomR (30, 50)
    fromString . (replicate dashcount '-' <>)
               . take charcount <$> getRandomRs ('0', 'Z') --wireshark complains about < and > in boundary, it works however.

field :: ByteString -> ByteString -> Field
field n v = Field [("name", n)] [] (toLBS v)

fieldNameIs :: ByteString -> Field -> Bool
fieldNameIs n f = maybe False (==n) $ lookup "name" $ fieldAttrs f

renderField :: ByteString -> Field -> LByteString
renderField boundary (Field params headers body) =
    "--" <> toLBS boundary <> "\r\n"
    <> "Content-Disposition: form-data" <> L.concat (map renderParam params)
    <> "\r\n" <> renderHeaders headers <> "\r\n"
    <> body <> "\r\n"
  where renderParam (n, v) = toLBS $ "; " <> n <> "=\"" <> v <> "\""
        renderHeader (header, val) = original header <> ": " <> val
        renderHeaders [] = mempty
        renderHeaders heads = L.concat (map (toLBS . renderHeader) heads) <> "\r\n"

formatMultipart :: ByteString -> [Field] -> RequestBody a
formatMultipart boundary fields =
    RequestBodyLBS $
        L.concat (map (renderField boundary) fields)
        <> "--" <> toLBS boundary <> "--\r\n" --finalizer
