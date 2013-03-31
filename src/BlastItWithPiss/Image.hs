module BlastItWithPiss.Image
    (Image(..)
    ,appendJunkB
    ,appendJunk
    ,readImageWithoutJunk
    ,mkImageFileName

    ,filterImages
    )where
import Import hiding (insert)

import BlastItWithPiss.MonadChoice

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Map (toDescList)

import System.FilePath (takeFileName, takeExtension)

import Network.Mime

data Image
    = Image
        {filename :: !String
        ,contentType :: !ByteString
        ,bytes :: !LByteString}
  deriving Show

instance NFData Image where
    rnf Image{..} = rnf (filename, contentType, bytes)

appendJunkB :: MonadChoice m => LByteString -> m LByteString
appendJunkB b = do
    bytecount <- getRandomR (2048, 20480)
    L.append b . L.pack . take bytecount <$> getRandomRs (1, 255)

appendJunk :: MonadChoice m => Image -> m Image
appendJunk i = do
    b <- appendJunkB (bytes i)
    return i{bytes=b}

readImageWithoutJunk :: MonadIO m => String -> m Image
readImageWithoutJunk fn = do
    bs <- liftIO $ L.readFile fn
    return Image{filename = takeFileName fn
                ,contentType = defaultMimeLookup $ fromString fn
                ,bytes = bs}

mkImageFileName :: MonadChoice m => MimeType -> m String
-- It's toDescList instead of toAscList because otherwise "image/jpg" returns ".jpe"
mkImageFileName ct = do
    name <- chooseFromList filenames
    return $ (++) name $
        maybe "" ("." ++) $ fmap T.unpack $ lookup ct $ map swap $ toDescList $
            defaultMimeMap
  where swap (a,b) = (b,a)

filenames :: [String]
filenames =
    ["captain-obvious"
    ,"nigra"
    ,"mint"
    ,"shinku"
    ,"Capibara"
    ,"kuruminha"
    ,"Kuruminha"
    ,"kukla"
    ,"1353246364"
    ,"1348524534"
    ,"1348124565"
    ,"1235623664"
    ,"2010"
    ,"nyasha"
    ,"Nyashka"
    ,"UnamusedCat"
    ,"lolcat-235"
    ,"apocalypse-now"
    ,"ololo"
    ,"dvach"
    ]

filterImages :: [FilePath] -> [FilePath]
filterImages = filter ((`elem` [".jpg",".jpeg",".jpe",".gif",".png"]) . takeExtension)
