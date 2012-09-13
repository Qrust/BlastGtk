{-# LANGUAGE NoOverloadedStrings #-}
module BlastItWithPiss.Parsing where
import Import
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

parseSpeed :: [Tag String] -> Int
parseSpeed t = getSpeed (parseSpeed' False t <|> parseSpeed' True t)
  where getSpeed mtext =
-- FIXME seems that sosaka hides speed sometimes
            fromMaybe 0 $
                readMay . takeWhile (not . isSpace) =<<
                    stripPrefix "[Скорость борды: " =<< mtext
        parseSpeed' uncommented tags =
            case getInfixOfP
                    [(~== if uncommented
                            then TagOpen "div" [("class", "speed")]
                            else TagComment "<div class=\"speed\">")
                    ,maybe False (isInfixOf "Скорость борды") . maybeTagText
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
        fromMaybe (error "parsePages: couldn't find current page") (findMap aux ts)
            >$> \c -> (c, maximum (c : mapMaybe readMay ts))
  where aux t = if '[' `elem` t
                    then readMay $ filter (`notElem` "[]") t
                    else Nothing

parsePage :: [Tag String] -> Page
parsePage html =
    let (i, ps) = parsePages html in
    Page {pageId = i
         ,lastpage = ps
         ,speed = parseSpeed html
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
