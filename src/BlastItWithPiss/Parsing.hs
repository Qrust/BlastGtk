module BlastItWithPiss.Parsing
    (Html

    ,Post(..)
    ,Thread(..)
    ,Page(..)
    ,postsFromPage

    ,parseOpPost
    ,parsePosts
    ,parseThreads
    ,parseSpeed
    ,parsePages
    ,parsePage

    ,ErrorMessage(..)
    ,ErrorException(..)
    ,Outcome(..)
    ,message
    ,successOutcome
    ,wordfiltered
    ,haveAnError
    ,cloudflareCaptcha
    ,cloudflareBan
    ,detectOutcome
    ,detectCloudflare

    ,parseForm
    -- ,conduitParseForm
    ) where
import Import
import BlastItWithPiss.Board
import qualified Text.Show as S

import Text.HTML.TagSoup

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.HTTP.Conduit.MultipartFormData

{-
import qualified Text.HTML.TagStream.Types as G
import Text.HTML.TagStream.Text hiding (decode)
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Conduit.Text (decode, utf8)
import Data.XML.Types
import Text.HTML.DOM
-}

type Html = [Tag Text]

tgOpen :: Text -> [(Text, Text)] -> Tag Text
tgOpen = TagOpen

tgClose :: Text -> Tag Text
tgClose = TagClose

data Post = Post
    {postId :: Int
    ,postContents :: String
    }
    deriving (Eq, Ord)

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

instance Show Post where
    show (Post postid _) = "Post " ++ show postid

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
    rnf Post{..} = rnf (postId, postContents)

instance NFData Thread where
    rnf Thread{..} = rnf (threadId, pinned, locked, postcount, visibleposts)

instance NFData Page where
    rnf Page{..} = rnf (pageId, lastpage, speed, threads)

postsFromPage :: Page -> [Post]
postsFromPage = concatMap visibleposts . threads

innerTextWithBr :: [Tag Text] -> String
innerTextWithBr = concat . mapMaybe aux
    where aux (TagOpen "br" []) = Just "\n"
          aux a = T.unpack <$> maybeTagText a

parseOpPost :: Int -> [Tag Text] -> (Post, [Tag Text])
parseOpPost i ts =
    let (postcont, rest) = break (==TagClose "blockquote") ts
    in (Post i $ innerTextWithBr postcont, tailSafe rest)

parsePosts :: [Tag Text] -> ([Post], [Tag Text])
parsePosts = first reverse . go []
  where strip'm' ('m':a) = Just a
        strip'm' _ = Nothing
        go posts (TagOpen "blockquote" (("id", mpostid):_):ts) =
            let (postcont, (_:rest)) = break (==TagClose "blockquote") ts
            in case (flip Post $ innerTextWithBr postcont) <$>
                        (readMay =<< strip'm' (T.unpack mpostid)) of
                Just a -> go (a:posts) rest
                Nothing -> go posts rest
        go posts a@(t:ts)
            | t == TagOpen "br" [("style", "clear:left;")] = (posts, a)
            | otherwise = go posts ts
        go posts [] = (posts, [])

parseOmitted :: [Tag Text] -> (Maybe Int, [Tag Text])
parseOmitted (TagText x:ts) =
    let t = dropWhile isSpace $ T.unpack x
    in case (readMay . takeUntil isSpace =<< stripPrefix "Пропущено " t) <|>
                (readMay $ takeUntil isSpace t) of
        Just a -> (Just a, ts)
        Nothing -> parseOmitted ts
parseOmitted (TagOpen "script" _:ts) = (Nothing, ts)
parseOmitted (_:ts) = parseOmitted ts
parseOmitted [] = (Nothing, [])

parseIcons :: (Bool, Bool) -> [Tag Text] -> ((Bool, Bool), [Tag Text])
parseIcons (pin,lck) (TagOpen "img" ats:ts)
            | Just "/sticky.png" <- lookup "src" ats = parseIcons (True, lck) ts
            | Just "/locked.png" <- lookup "src" ats = parseIcons (pin, True) ts
parseIcons (pin,lck) (TagOpen "blockquote" _:ts) = ((pin, lck), ts)
parseIcons (pin,lck) (_:ts) = parseIcons (pin,lck) ts
parseIcons (pin,lck) [] = parseIcons (pin,lck) []

parseThreads :: [Tag Text] -> ([Thread], [Tag Text])
parseThreads = first reverse . go []
  where go tds (TagOpen "div" (("id", postid):_):ts)
            | Just tid <- readMay . T.unpack =<< T.stripPrefix "thread_" postid
             ,((pin,lck),rest1) <- parseIcons (False, False) ts
             ,(oppost, rest2) <- parseOpPost tid rest1
             ,(mpostn, rest3) <- parseOmitted rest2
             ,(vposts, rest4) <- parsePosts rest3
             = go (Thread {threadId = tid
                          ,postcount = fromMaybe 0 mpostn + length vposts
                          ,visibleposts = oppost : vposts
                          ,pinned = pin
                          ,locked = lck
                          } : tds) rest4
        go tds (TagOpen "table" [("border","1")]:TagOpen "tbody" []:TagOpen "tr" []:TagOpen "td" []:ts) =
            (tds, ts)
        go tds (_:ts) = go tds ts
        go tds [] = (tds, [])

parseSpeed :: [Tag Text] -> Maybe Int
parseSpeed t = getSpeed =<< parseSpeed' t
  where stripSpeedPrefix a = stripPrefix "[Скорость борды: " (dropWhile isSpace a)
                         <|> stripPrefix "[Posting speed: " (dropWhile isSpace a)
        getSpeed = readMay . takeUntil isSpace
        parseSpeed' =
            stripSpeedPrefix . T.unpack . innerText .
                dropUntil (== tgOpen "p" [("class", "footer")])

parsePages :: [Tag Text] -> ((Int, Int), [Tag Text])
parsePages tags
    | (work, (_:rest)) <- break (== tgClose "tbody") tags
    , texts <- filter (T.any isNumber) $ mapMaybe maybeTagText work
    , current <- fromMaybe 0 (findMap extract texts)
    , others <- maximum (current : mapMaybe (readMay . T.unpack) texts)
        = ((current, others), rest)
    | otherwise
        = ((0, 0), tags)
  where extract t = if T.isInfixOf "[" t
                        then readMay $ takeWhile isNumber $ dropUntil isNumber $ T.unpack t
                        else Nothing

parsePage :: Board -> [Tag Text] -> Page
parsePage board html =
    let (tds, rest1) = parseThreads html
        ((i, lp), rest2) = parsePages rest1
         -- if parse fails assume last recorded speed, or 0 if none recorded.
        spd = flip fromMaybe (parseSpeed rest2) $
                fromMaybe 0 $ lookup board ssachBoardsSortedByPostRate
    in
    Page {pageId = i
         ,lastpage = lp
         ,speed = spd
         ,threads = tds
         }

-- Only valid within one board.
parseForm :: (Monad m, Monad m') => String -> [Tag Text] -> (String, [Part m m'])
parseForm host tags =
    let (f:html) = dropUntil (~== tgOpen "form" [("id", "postform")]) tags
    in  (getWakabaPl f
        ,map inputToField . filter aux $ takeUntil (~== tgClose "form") html
        )
  where
    getWakabaPl f =
        host <> T.unpack (fromAttrib "action" f)
    aux t
      | t ~== tgOpen "input" [("type", "radio")]
        = fromAttrib "checked" t == "checked"
      | otherwise
        = t ~== tgOpen "input" [("name", "")]
    inputToField tag =
        partBS (fromAttrib "name" tag) (T.encodeUtf8 $ fromAttrib "value" tag)

newtype ErrorMessage = Err {unErrorMessage :: String}
    deriving (Eq, Ord)

instance Show ErrorMessage where
    show = unErrorMessage -- show cyrillic as is, instead of escaping it.

newtype ErrorException = ErrorException {unErrorException :: SomeException}
    deriving (Typeable)

instance Show ErrorException where
    show = show . unErrorException

instance Eq ErrorException where
    (==) _ _ = True

data Outcome = Success
             | SuccessLongPost {rest :: String}
             | Wordfilter
             | Banned {errMessage :: ErrorMessage}
             | SameMessage
             | SameImage
             | TooFastPost
             | TooFastThread
             | NeedCaptcha
             | WrongCaptcha
             | RecaptchaBan
             | LongPost
             | EmptyPost
             | CorruptedImage
             | PostRejected
             | CloudflareCaptcha
             | CloudflareBan
             | Four'o'FourBan
             | Four'o'ThreeBan
             | Five'o'ThreeError
             | OtherError {errMessage :: ErrorMessage}
             | InternalError {errException :: ErrorException}
             | UnknownError
    deriving (Eq, Show)

instance NFData ErrorMessage where
    rnf = rnf . unErrorMessage

instance NFData ErrorException

instance NFData Outcome where
    rnf (SuccessLongPost s) = rnf s
    rnf (Banned s) = rnf s
    rnf (OtherError s) = rnf s
    rnf (InternalError s) = rnf s
    rnf a = a `seq` ()

message :: Outcome -> String
message = unErrorMessage . errMessage

successOutcome :: Outcome -> Bool
successOutcome Success = True
successOutcome (SuccessLongPost _) = True
successOutcome _ = False

wordfiltered :: [Tag Text] -> Bool
wordfiltered =
    isPrefixOf [TagOpen "html" [], TagOpen "body" [], TagOpen "h1" [],TagText "Spam detected."]

haveAnError :: [Tag Text] -> Maybe Text
haveAnError tags =
    fromTagText . last <$> getInfixOfP
        [(~== tgOpen "center" [])
        ,(~== tgOpen "strong" [])
        ,(~== tgOpen "font" [("size", "5")])
        ,isTagText
        ] tags

cloudflareCaptcha :: [Tag Text] -> Bool
cloudflareCaptcha =
    isInfixOf [tgOpen "title" [], TagText "Attention required!"]

cloudflareBan :: [Tag Text] -> Bool
cloudflareBan =
    isInfixOfP [(==tgOpen "title" []), maybe False (T.isPrefixOf "Access Denied |") . maybeTagText]

detectCloudflare :: [Tag Text] -> Maybe Outcome
detectCloudflare tags
    | cloudflareCaptcha tags = Just CloudflareCaptcha
    | cloudflareBan tags = Just CloudflareBan
    | otherwise = Nothing

detectOutcome :: [Tag Text] -> Outcome
detectOutcome tags
    | wordfiltered tags = Wordfilter
    | Just err <- haveAnError tags =
        case () of
            _ | Just reason <- T.stripPrefix "Ошибка: Доступ к отправке сообщений с этого IP закрыт. Причина: " err
                -> Banned (Err $ T.unpack reason)
              | T.isInfixOf "String refused" err
                -> Wordfilter
              | T.isInfixOf "Флудить нельзя" err
                -> SameMessage
              | T.isInfixOf "Этот файл уже был загружен" err
                -> SameImage
              | T.isInfixOf "Обнаружен флуд" err
                -> TooFastPost
              | T.isInfixOf "Вы уже создали один тред" err
                -> TooFastThread
              | T.isInfixOf "забыли ввести капчу" err
                -> NeedCaptcha
              | T.isInfixOf "Неверный код подтверждения" err
                -> WrongCaptcha
              | T.isInfixOf "заблокирован на сервере ReCaptcha" err
                || T.isInfixOf "Your IP probably was blocked on Recaptcha server" err
                -> RecaptchaBan
              | T.isInfixOf "Слишком большое сообщение" err
                -> LongPost
              | T.isInfixOf "Загружаемый вами тип файла не поддерживается" err
                -> CorruptedImage
              | T.isInfixOf "ничего не написали в сообщении" err
                -> EmptyPost
              | otherwise
                -> OtherError (Err $ T.unpack err)
    | otherwise = UnknownError
{-
awaitUntil :: Monad m => (i -> Bool) -> GSink i m (Maybe i)
awaitUntil p = loop
  where loop = do
            x <- await
            case x of
                Just x -> if p x then return (Just x) else loop
                a -> return a

{-- # SPECIALIZE (~~==) :: G.Token' Text -> G.Token' Text -> Bool #-}
{-- # SPECIALIZE (~~==) :: G.Token' ByteString -> G.Token' ByteString -> Bool #-}
(~~==) :: (Monoid a, Eq a) => G.Token' a -> G.Token' a -> Bool
(~~==) a b = f a b
    where
        strNull x = x == mempty
        f (G.Text y) (G.Text x) = strNull x || x == y
        f (G.TagClose y) (G.TagClose x) = strNull x || x == y
        f (G.TagOpen y ys _) (G.TagOpen x xs _) = (strNull x || x == y) && all g xs
            where
                g (name,val) | strNull name = val  `elem` map snd ys
                             | strNull val  = name `elem` map fst ys
                g nameval = nameval `elem` ys
        f _ _ = False

fc :: [Content] -> Text
fc [ContentText a] = a

conduitParseForm :: (MonadResource m, MonadBaseControl IO m) => String -> ResumableSource m ByteString -> m (String, [Field])
conduitParseForm host http =
    http $$+- eventConduit =$ do
        awaitForm
  where awaitForm = do
            Just (EventBeginElement "form" ats) <- awaitUntil (\x -> case x of (EventBeginElement "form" _ ) -> True; _ -> False)
            maybe awaitForm (`awaitFields` []) $ do
                guard =<< (=="postform") . fc <$> lookup "id" ats
                (host ++) . T.unpack . fc <$> lookup "action" ats
        awaitFields wakabapl fields = do
            Just x <- await
            case x of
                EventEndElement "form" -> return (wakabapl, fields)
                (EventBeginElement "input" ats) ->
                    maybe (awaitFields wakabapl fields)
                        (awaitFields wakabapl . (fields ++) . (:[])) $ do
                            case fc <$> lookup "type" ats of
                                Just "radio" -> do
                                    guard =<< (=="checked") . fc <$> lookup "checked" ats
                                    return (inputToField ats)
                                _ -> do
                                    guard $ isJust $ lookup "name" ats
                                    return (inputToField ats)
                _ -> awaitFields wakabapl fields
        inputToField :: [(Name, [Content])] -> Field
        inputToField ats =
            field (T.encodeUtf8 $ fromMaybe "" $ fc <$> lookup "name" ats)
                  (T.encodeUtf8 $ fromMaybe "" $ fc <$> lookup "value" ats)
-}
