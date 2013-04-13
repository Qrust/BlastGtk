module BlastItWithPiss.Video
    (readVideoFile
    ,parseVideo
    ) where
import Import

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

parseVideo :: Text -> Maybe [Text]
parseVideo t =
    case filter (not . T.null) $ T.lines t of
      [] -> Nothing
      a -> Just a

readVideoFile :: FilePath -> IO (Maybe [Text])
readVideoFile f = do
    fromIOException (return Nothing) $ do
        withFile f ReadMode $ \h -> do
            hSetNewlineMode h universalNewlineMode
            parseVideo <$> TIO.hGetContents h
