{-# LANGUAGE NoOverloadedStrings #-}
module BlastItWithPiss.Parsing where
import Import hiding (fromString)
import BlastItWithPiss.Board
import BlastItWithPiss.MultipartFormData (Field(..), field)
import Text.HTML.TagSoup
import Text.StringLike
import qualified Codec.Binary.UTF8.Generic as UTF8
-- html-conduit(cursor?)?

data Post = Post
    {postId :: Int
    ,postContents :: String
    }
    deriving (Show, Eq, Ord)

data Thread = Thread
    {threadId :: Int
    ,pinned :: Bool
    ,locked :: Bool
    ,postcount :: Int
    ,visibleposts :: [Post]
    }
    deriving (Show, Eq)

data Page = Page
    {pageId :: Int
    ,lastpage :: Int
    ,speed :: Int
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

innerTextWithBr :: StringLike a => [Tag a] -> a
innerTextWithBr = strConcat . mapMaybe aux
    where aux (TagOpen x []) | x == fromString "br" = Just (fromChar '\n')
          aux a = maybeTagText a

parsePosts :: [Tag String] -> [Post]
parsePosts posts =
    mapMaybe readPost $ 
        sections (~== TagOpen "blockquote" [("class", "postMessage")])
                 posts
  where readPost (TagOpen "blockquote" (("id", 'm':postid):_):rest) =
            Post <$> readMay postid <*>
                pure (innerTextWithBr $ takeUntil (==TagClose "blockquote") rest)
        readPost _ = Nothing

parseThreadParameters :: [Tag String] -> Thread
parseThreadParameters withOp@(TagOpen "div" (("id", postid):_):thrd)
    | Just tid <- readMay =<< stripPrefix "thread_" postid
     ,pin <- any (~== TagOpen "img" [("src", "/sticky.png")]) thrd
     ,lck <- any (~== TagOpen "img" [("src", "/locked.png")]) thrd
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
                (readMay . takeUntil isSpace =<< stripPrefix "Пропущено " (dropWhile isSpace x))
            <|> (readMay $ takeUntil isSpace $ dropWhile isSpace x) --int
        omittedCount _ = Nothing
parseThreadParameters thrd = error $ "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n" ++ show thrd

parseThread :: [Tag String] -> Thread
parseThread =
    parseThreadParameters . tail . dropUntil (~== TagOpen "div" [("class", "thread")])

parseSpeed :: [Tag String] -> Maybe Int
parseSpeed t = getSpeed (parseSpeed' t)
-- FIXME seems that sosaka hides speed sometimes
  where stripSpeedPrefix a = stripPrefix "[Скорость борды: " a
                         <|> stripPrefix "[Posting speed: " a
        isInfixOfSpeed a = isInfixOf "Скорость борды" a
                        || isInfixOf "Posting speed" a
        getSpeed mtext =
                readMay . takeWhile (not . isSpace) =<<
                    stripSpeedPrefix =<< mtext
        parseSpeed' tags =
            fromTagText . last <$>
                getInfixOfP
                    [\x -> x ~== TagOpen "div" [("class", "speed")]
                        || x ~== TagComment "<div class=\"speed\">"
                    ,maybe False isInfixOfSpeed . maybeTagText
                    ]
                    tags

parsePages :: [Tag String] -> (Int, Int)
parsePages =
-- FIXME seems that sosaka hides pages sometimes
    dropUntilLP [(~== TagOpen "table" [("border", "1")])
                 ,(== TagOpen "tbody" [])
                 ,(== TagOpen "tr" [])
                 ,(== TagOpen "td" [])
                 ] >>>
        takeUntil (== TagClose "tbody") >>>
            mapMaybe maybeTagText >>>
                \ts -> fromMaybe 0 (findMap aux ts) >$>
                    \c -> (c, maximum (c : mapMaybe readMay ts))
  where aux t = if '[' `elem` t
                    then readMay $ filter (`notElem` "[]") t
                    else Nothing

parsePage :: Board -> [Tag String] -> Page
parsePage board html =
    let (i, ps) = parsePages html in
    Page {pageId = i
         ,lastpage = ps
         -- if parse fails assume last recorded speed, or 0 if none recorded.
         ,speed = fromMaybe (fromMaybe 0 $ lookup board ssachBoardsSortedByPostRate) $
                    parseSpeed html
         ,threads = map parseThreadParameters $
                        partitions (~== TagOpen "div" [("class", "thread")]) html
         }

-- Only valid within one board.
parseForm :: String -> [Tag String] -> (String, [Field])
parseForm host tags =
    dropUntil (~== TagOpen "form" [("id", "postform")]) tags >$>
        \(f:html) -> (getWakabaPl f,
                        map inputToField . filter aux $
                            takeUntil (~== TagClose "form") html
                     )
  where getWakabaPl f = host <> fromAttrib "action" f
        aux t | t ~== TagOpen "input" [("type", "radio")]
                = fromAttrib "checked" t == "checked"
              | otherwise
                = t ~== TagOpen "input" [("name", ""){-, ("value", "")-}]
        inputToField tag =
            field (UTF8.fromString $ fromAttrib "name" tag)
                  (UTF8.fromString $ fromAttrib "value" tag)

