module BlastItWithPiss.Image
    (Image(..)
    ,readImageWithoutJunk

    ,JunkImage(..)
    ,appendJunk

    ,mkImageFileName
    ,filterImages
    ,appendJunkB
    )where
import Import hiding (insert)

import BlastItWithPiss.MonadChoice

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Map (toDescList)

import System.FilePath (takeFileName, takeExtension)

import Network.Mime

newtype JunkImage = JunkImage {fromJunkImage :: Image}
  deriving (Show, Eq)

instance NFData JunkImage where
    rnf = rnf . fromJunkImage

data Image = Image
    {filename    :: !String
    ,contentType :: !ByteString
    ,bytes       :: !LByteString}
  deriving (Show, Eq)

instance NFData Image where
    rnf (Image a b c) = rnf (a, b, c)

appendJunkB :: MonadChoice m => LByteString -> m LByteString
appendJunkB img = do
    bytecount <- getRandomR (2048, 20480)
    (img `L.append`) . L.pack . take bytecount <$> getRandomRs (1, 255)

appendJunk :: MonadChoice m => Image -> m JunkImage
appendJunk i = do
    b <- appendJunkB (bytes i)
    return $ JunkImage i{bytes=b}

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
    ,"228"
    ]

filterImages :: [FilePath] -> [FilePath]
filterImages = filter ((`elem` [".jpg",".jpeg",".jpe",".gif",".png"]) . takeExtension)
