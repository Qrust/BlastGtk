{-# LANGUAGE NoOverloadedStrings #-}
module BlastItWithPiss.Parsing where
import Import
import BlastItWithPiss.Board
import BlastItWithPiss.MultipartFormData (Field(..), field)
import Text.HTML.TagSoup
import qualified Codec.Binary.UTF8.Generic as UTF8
-- html-conduit(cursor?)?

newtype Post = Post
    {postId :: Int
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

parsePosts :: [Tag String] -> [Post]
parsePosts posts = catMaybes
    (filter
        (\x -> x ~== TagOpen "div" [("class","oppost")]
            || x ~== TagOpen "td" [("class","reply")]) posts >$> map readPost)
  where readPost (TagOpen "div" (("id", postid):_)) =
            Post <$> (readMay =<< stripPrefix "post_" postid)
        readPost (TagOpen "td" [_,("id",postid)]) =
            Post <$> readMay postid
        readPost _ = Nothing

parseThreadParameters :: [Tag String] -> Thread
parseThreadParameters (TagOpen "div" (("id", postid):_):thrd)
    | Just tid <- readMay =<< stripPrefix "post_" postid
     ,pin <- any (~== TagOpen "img" [("src", "/sticky.png")]) thrd
     ,lck <- any (~== TagOpen "img" [("src", "/locked.png")]) thrd
     ,vposts <- parsePosts thrd
     ,postn <- fromMaybe 0 (processpost =<< find (\t -> fromMaybe False $ isPrefixOf "Пропущено " <$> maybeTagText t) thrd)
             + length vposts
     = Thread {threadId = tid
              ,postcount = postn
              ,visibleposts = vposts
              ,pinned = pin
              ,locked = lck
              }
  where processpost (TagText x) = readMay $ takeUntil isSpace $ fromJust $ stripPrefix "Пропущено " x
        processpost _ = Nothing
parseThreadParameters thrd = error $ "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n" ++ show thrd

parseThread :: [Tag String] -> Thread
parseThread =
    parseThreadParameters . tail . dropUntil (~== TagOpen "div" [("class", "thread")])

parseSpeed :: [Tag String] -> Maybe Int
parseSpeed t = getSpeed (parseSpeed' False t <|> parseSpeed' True t)
  where stripSpeedPrefix a = stripPrefix "[Скорость борды: " a
                         <|> stripPrefix "[Posting speed: " a
        isInfixOfSpeed a = isInfixOf "Скорость борды" a
                         || isInfixOf "Posting speed" a
        getSpeed mtext =
-- FIXME seems that sosaka hides speed sometimes
                readMay . takeWhile (not . isSpace) =<<
                    stripSpeedPrefix =<< mtext
        parseSpeed' uncommented tags =
            case getInfixOfP
                    [(~== if uncommented
                            then TagOpen "div" [("class", "speed")]
                            else TagComment "<div class=\"speed\">")
                    ,maybe False isInfixOfSpeed . maybeTagText
                    ] tags
            of Nothing -> Nothing
               Just ts -> Just $ fromTagText $ last ts

parsePages :: [Tag String] -> (Int, Int)
parsePages =
    (mapMaybe maybeTagText .
        takeUntil (== TagClose "tbody") .
            dropUntilLP [(~== TagOpen "table" [("border", "1")])
                        ,(== TagOpen "tbody" []), (== TagOpen "tr" [])
                        ,(== TagOpen "td" [])]
    ) >>> \ts ->
        --fromMaybe (error "parsePages: couldn't find current page") (findMap aux ts)
-- FIXME seems that sosaka hides pages sometimes
        fromMaybe 0 (findMap aux ts) >$>
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
         ,speed = parseSpeed html >$>
                    fromMaybe (fromMaybe 0 $ lookup board ssachBoardsSortedByPostRate)
         ,threads = map parseThreadParameters $ tail $ splitBy (~== TagOpen "div" [("class", "thread")]) html
         }

-- Only valid for one board.
parseForm :: String -> [Tag String] -> (String, [Field])
parseForm host tags =
    dropUntil (~== TagOpen "form" [("id", "postform")]) tags
    >$> \(f:html) ->
        (getWakabaPl f, takeUntil (~== TagClose "form") html
                        >$> map inputToField . filter aux)
  where getWakabaPl f = host <> fromAttrib "action" f
        aux t | t ~== TagOpen "input" [("type", "radio")]
                = fromAttrib "checked" t == "checked"
              | otherwise
                = t ~== TagOpen "input" [("name", ""){-, ("value", "")-}]
        inputToField tag =
            field (UTF8.fromString $ fromAttrib "name" tag)
                  (UTF8.fromString $ fromAttrib "value" tag)

--test :: IO ()
--test = undefined
