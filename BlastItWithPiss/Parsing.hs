module BlastItWithPiss.Parsing where
import Import hiding (fromString)
import BlastItWithPiss.Board
import BlastItWithPiss.MultipartFormData (Field(..), field)
import Text.HTML.TagSoup
import Text.StringLike
-- html-conduit(cursor?)?

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding as ST

tgOpen :: T.Text -> [(T.Text, T.Text)] -> Tag T.Text
tgOpen = TagOpen

tgClose :: T.Text -> Tag T.Text
tgClose = TagClose

tgComment :: T.Text -> Tag T.Text
tgComment = TagComment

data Post = Post
    {postId :: !Int
    ,postContents :: !String
    }
    deriving (Show, Eq, Ord)

data Thread = Thread
    {threadId :: !Int
    ,pinned :: !Bool
    ,locked :: !Bool
    ,postcount :: !Int
    ,visibleposts :: [Post]
    }
    deriving (Show, Eq)

data Page = Page
    {pageId :: !Int
    ,lastpage :: !Int
    ,speed :: !Int
    ,threads :: [Thread]
    }
    deriving (Show, Eq)

instance Ord Thread where
    compare x y = compare (postcount x) (postcount y)

instance Ord Page where
    compare x y = compare (pageId x) (pageId y)

instance NFData t => NFData (Tag t) where
    rnf (TagOpen x y) = x `deepseq` y `deepseq` ()
    rnf (TagClose x) = rnf x
    rnf (TagText x) = rnf x
    rnf (TagComment x) = rnf x
    rnf (TagWarning x) = rnf x
    rnf (TagPosition x y) = x `deepseq` y `deepseq` ()

instance NFData Post where
    rnf Post{..} = rnf postId

instance NFData Thread where
    rnf Thread{..} = rnf (threadId, pinned, locked, postcount, visibleposts)

instance NFData Page where
    rnf Page{..} = rnf (pageId, lastpage, speed, threads)

innerTextWithBr :: [Tag T.Text] -> T.Text
innerTextWithBr = T.concat . mapMaybe aux
    where aux (TagOpen x []) | x == "br" = Just "\n"
          aux a = maybeTagText a

parsePosts :: [Tag T.Text] -> [Post]
parsePosts posts =
    mapMaybe readPost $ 
        sections (~== tgOpen "blockquote" [("class", "postMessage")]) posts
  where readPost :: [Tag T.Text] -> Maybe Post
        readPost (TagOpen "blockquote" (("id", mpostid):_):rest) =
            Post <$> (readMay . T.unpack =<< T.stripPrefix "m" mpostid) <*>
                pure (T.unpack $ innerTextWithBr $ takeUntil (== tgClose "blockquote") rest)
        readPost _ = Nothing

parseThreadParameters :: [Tag T.Text] -> Thread
parseThreadParameters withOp@(TagOpen "div" (("id", postid):_):thrd)
    | Just tid <- readMay . T.unpack =<< T.stripPrefix "thread_" postid
     ,pin <- any (~== tgOpen "img" [("src", "/sticky.png")]) thrd
     ,lck <- any (~== tgOpen "img" [("src", "/locked.png")]) thrd
     ,vposts <- parsePosts withOp
     ,postn <- fromMaybe 0 (subtract 1 <$> findMap omittedCount thrd)
             + length vposts
     = Thread {threadId = tid
              ,postcount = postn
              ,visibleposts = vposts
              ,pinned = pin
              ,locked = lck
              }
  where omittedCount (TagText x) =
                (readMay . takeUntil isSpace . T.unpack =<< T.stripPrefix "Пропущено " (T.stripStart x))
            <|> (readMay $ takeUntil isSpace $ T.unpack x) --int
        omittedCount _ = Nothing
parseThreadParameters thrd = error $ "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n" ++ show thrd

parseThread :: [Tag T.Text] -> Thread
parseThread =
    parseThreadParameters . tail . dropUntil (~== tgOpen "div" [("class", "thread")])

parseSpeed :: [Tag T.Text] -> Maybe Int
parseSpeed t = getSpeed (parseSpeed' t)
-- FIXME seems that sosaka hides speed sometimes
  where stripSpeedPrefix a = T.stripPrefix "[Скорость борды: " (T.stripStart a)
                         <|> T.stripPrefix "[Posting speed: " (T.stripStart a)
        getSpeed mtext =
            readMay . takeUntil isSpace . T.unpack =<< mtext
        parseSpeed' =
            stripSpeedPrefix <=< maybeTagText <=< headMay . tailSafe .
                dropUntil (\x -> x ~== tgOpen "div" [("class", "speed")]
                              || x ~== tgComment "<div class=\"speed\">")

parsePages :: [Tag T.Text] -> (Int, Int)
parsePages =
-- FIXME seems that sosaka hides pages sometimes
    dropUntilLP [(~== tgOpen "table" [("border", "1")])
                 ,(== tgOpen "tbody" [])
                 ,(== tgOpen "tr" [])
                 ,(== tgOpen "td" [])
                 ] >>>
        takeUntil (== tgClose "tbody") >>>
            mapMaybe maybeTagText >>>
                \ts -> fromMaybe 0 (findMap aux ts) >$>
                    \c -> (c, maximum (c : mapMaybe (readMay . T.unpack) ts))
  where aux t = if "[" `T.isInfixOf` t
                    then readMay $ T.unpack $ T.filter (`notElem` "[]") t
                    else Nothing

parsePage :: Board -> [Tag T.Text] -> Page
parsePage board html =
    let (i, ps) = parsePages html in
    Page {pageId = i
         ,lastpage = ps
         -- if parse fails assume last recorded speed, or 0 if none recorded.
         ,speed = fromMaybe (fromMaybe 0 $ lookup board ssachBoardsSortedByPostRate) $
                    parseSpeed html
         ,threads = map parseThreadParameters $
                        partitions (~== tgOpen "div" [("class", "thread")]) html
         }

-- Only valid within one board.
parseForm :: String -> [Tag T.Text] -> (String, [Field])
parseForm host tags =
    dropUntil (~== tgOpen "form" [("id", "postform")]) tags >$>
        \(f:html) -> (getWakabaPl f,
                        map inputToField . filter aux $
                            takeUntil (~== tgClose "form") html
                     )
  where getWakabaPl f = host <> T.unpack (fromAttrib "action" f)
        aux t | t ~== tgOpen "input" [("type", "radio")]
                = fromAttrib "checked" t == "checked"
              | otherwise
                = t ~== tgOpen "input" [("name", ""){-, ("value", "")-}]
        inputToField tag =
            field (ST.encodeUtf8 $ T.toStrict $ fromAttrib "name" tag)
                  (T.encodeUtf8 $ fromAttrib "value" tag)

