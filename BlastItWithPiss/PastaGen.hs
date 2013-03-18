module BlastItWithPiss.PastaGen
    (readPastaFile
    ,parsePasta

    ,randomQuote
    ,genPastaRandomQuote

    ,choosePostToRepost
    ,probablyDescendAndGetPosts
    ,genPastaFromReposts
    ) where
import Import

import BlastItWithPiss.Parsing
import BlastItWithPiss.MonadChoice

import qualified Data.Text as T
import qualified Data.ByteString as B

parsePasta :: String -> Maybe [String]
parsePasta s =
    case filter (\x -> not $ all isSpace x || null x) $ delimitByLE "\n\n\n\n" s of
        [] -> Nothing
        a -> Just a

readPastaFile :: FilePath -> IO (Maybe [String])
readPastaFile f =
    fromIOException (return Nothing) $
        parsePasta . T.unpack . decodeUtf8 <$> B.readFile f

probablyDescendAndGetPosts
    :: MonadChoice m
    => Rational
    -> Rational
    -> (Int -> m Thread)
    -> Maybe Page
    -> Maybe Int
    -> m [Post]
probablyDescendAndGetPosts _ _ _ Nothing Nothing = return []
probablyDescendAndGetPosts _ _ _ (Just p0) Nothing = return $ postsFromPage p0
probablyDescendAndGetPosts _ _ getThread Nothing (Just tid) = visibleposts <$> getThread tid
probablyDescendAndGetPosts pprob tprob getThread (Just p0) (Just tid) = do
    fromThread <- fromList [(False, pprob), (True, tprob)]
    if fromThread
      then visibleposts <$> getThread tid
      else return $ postsFromPage p0

randomQuote :: MonadChoice m => [Post] -> String -> m String
randomQuote [] msg =
    return msg
randomQuote posts msg = do
    post <- chooseFromList posts
    let puremsg = initSafe $ unlines $ removequotes $ lines msg -- initSafe removes trailing newline from unlines
    let quote =
          unlines $
            (">>" ++ show (postId post)) :
              map ('>' :) (removequotes $ lines $ postContents post)
    return $ quote ++ puremsg
  where
    removequotes = filter (fromMaybe True . fmap (/='>') . headMay)

choosePostToRepost :: MonadChoice m => Bool -> [Post] -> m String
choosePostToRepost randomquote posts = do
    msg <- fmap (fromMaybe "") $ chooseFromListMaybe $ filter (not . null) $ map postContents posts
    if randomquote
      then randomQuote posts msg
      else return msg

-- | Randomly choose a post to quote and append to msg
genPastaRandomQuote :: MonadChoice m
                    => Rational -- ^ Probability of quoting a reply
                    -> Rational -- ^ Probability of quoting a thread
                    -> (Int -> m Thread) -- ^ Get thread
                    -> Maybe Page -- ^ Maybe front page
                    -> Maybe Int -- ^ Maybe current thread number
                    -> String -- ^ msg
                    -> m String
genPastaRandomQuote pprob tprob getThread mp0 mtid msg = do
    quotePosts <- probablyDescendAndGetPosts pprob tprob getThread mp0 mtid
    randomQuote quotePosts msg

-- | Randomly choose a post to repost from page or from thread
genPastaFromReposts :: MonadChoice m
                    => Bool -- ^ Whether to also append a random quote
                    -> (Int -> m Thread)
                    -> Maybe Page
                    -> Maybe Int
                    -> m String
genPastaFromReposts toQuote getThread mp0 mtid = do
    fromPosts <- probablyDescendAndGetPosts 10 90 getThread mp0 mtid
    choosePostToRepost toQuote fromPosts
