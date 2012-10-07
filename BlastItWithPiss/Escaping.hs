module BlastItWithPiss.Escaping
    (
    -- * High level
    escape

    -- * Constants
    ,wordfilter
    ,invisibleSymbols
    ,simillars
    ,findAlternative

    -- * Functions
    ,randomizeLang
    ,randomizeInvisible
    ,randomizeOneCharLang
    ,escapeOneWord
    ,escapeWordfilter
    ) where
import Import
import BlastItWithPiss.MonadChoice

-- FIXME code here is a goddamn mess.

-- | Words which need escaping
wordfilter :: [String]
wordfilter =
    ["сосач"
    ,"Сосач"
    ,"сомали"
    ,"Сомали"
    ,"тиреч"
    ,"Тиреч"
    ,"двач"
    ,"Двач"
    ,"колчан"
    ,"Колчан"
    ,"нульча"
    ,"Нульча"
    ,"0chan.ru"
    ,"1chan.ru"
    ,"2ch"
    ,"2-ch"
    ,"2--ch"
    ,"моч"
    ,"Моч"
    ,"моча"
    ,"Моча"
    ,"мочер"
    ,"Мочер"
    ,"педаль"
    ,"Педаль"
    ,"сосак"
    ,"Сосак"
    ,"макак"
    ,"Макак"
    ,"обезьян"
    ,"Обезьян"
    ,"абу"
    ,"Абу"
    ,"фишки"
    ,"яплакал"
    ,"conference.jabber"
    ,"Blast"
    ,"blast"
    ,"Piss"
    ,"piss"
    ,"github"
    ,"mdk"
    ,"MDK"
    ,"мдк"
    ]
-- FIXME "Fatal error: word \"" ++ str ++ "\" has no characters that can be substituted by equivalents."

-- | List of invisble symbols
invisibleSymbols :: [Char]
invisibleSymbols = "\8290\8291\8289\8288\8203\8206"

-- | TODO zalgo
zalgo :: [Char]
zalgo = []

-- | Simillar letters in Cyrillic, Latin and Greek.
simillars :: [(Char, Char)]
simillars =
    -- [(Latin/Greek, Cyrillic)]
    [('a', 'а')
    ,('A', 'А')
    ,('B', 'В')
    ,('Γ', 'Г')
    ,('c', 'с')
    ,('C', 'С')
    ,('e', 'е')
    ,('E', 'Е')
    ,('ë', 'ё')
    ,('Ë', 'Ё')
    ,('H', 'Н')
    ,('Η', 'Н')
    ,('i', 'і')
    ,('I', 'І')
    ,('Ï', 'Ї')
    ,('K', 'К')
    ,('M', 'М')
    ,('ᴎ', 'и')
    ,('o', 'о')
    ,('O', 'O')
    ,('p', 'р')
    ,('P', 'Р')
    ,('T', 'Т')
    ,('x', 'х')
    ,('X', 'Х')
    ,('϶', 'э')
    ,('ϵ', 'є')
    ,('ᴙ', 'я')
    ]

-- | Lookup 'simillars'
findAlternative :: Char -> Maybe Char
findAlternative c = findMap aux simillars
  where aux (a, b) | c==a = Just b
                   | c==b = Just a
                   | otherwise = Nothing

-- | Randomly substitute letters by simillar looking in different alphabet
randomizeLang :: MonadChoice m => String -> m String
randomizeLang str =
    forM str $ \c ->
        case findAlternative c of
            Just a -> ifM getRandom (return a) (return c) --depending on random we either substitute char, or leave it be.
            _ -> return c

insertAfterIxs :: [(Int, a)] -> [a] -> [a]
insertAfterIxs ixs as =
    case foldl' aux (0, [], as) ixs of
        (_, pas, as) -> concat (reverse pas) ++ as
  where aux (cur, pas, as) (ix, a) =
            case splitAt (ix-cur) as of
                (f, s) -> (ix, [a]:f:pas, s)

getIxs :: [Int] -> [a] -> [a]
getIxs is as = map (as!!) is --dangerous

-- | Intersperse string in random places with random amount of random invisible characters
randomizeInvisible :: MonadChoice m => Int -> String -> m String
randomizeInvisible max_invs' str = do
-- HACK as every one of our invisible chars consists of three unicode code points
--      we divide limit by three.
-- FIXME symbols should be counted by their UTF8 length in bytes.
    let max_invs = max_invs' `div` 3
    ixlen <- getRandomR (0, max_invs)
    ixs <- sort . take ixlen <$> getRandomRs (0, length str)
    syms <- flip getIxs invisibleSymbols . take ixlen <$>
                                    getRandomRs (0, length invisibleSymbols-1)
    return $ insertAfterIxs (zip ixs syms) str

randomizeOneCharLang :: MonadChoice m => String -> m String
randomizeOneCharLang str = do
    let ixs = findIndices (isJust . findAlternative) str
    if null ixs
        then return str -- FIXME $ error $ "Fatal error: word \"" ++ str ++ "\" has no characters that can be substituted by equivalents."
        else do (f, (v:s)) <- (`splitAt` str) <$> chooseFromList ixs
                return $ f ++ [fromJust (findAlternative v)] ++ s

escapeOneWord :: MonadChoice m => Int -> String -> m (Int, String)
escapeOneWord charsleft str = do
    cut_str <- randomizeInvisible charsleft str
    if str==cut_str
        then (,) 0 <$> randomizeOneCharLang str
        else do let charsused = length cut_str - length str
                ifM getRandom
                    ((,) charsused <$> randomizeOneCharLang cut_str)
                    (return (charsused, cut_str))

escapeWordfilter :: MonadChoice m => Int -> [String] -> String -> m String
escapeWordfilter charsleft wordfilter str =
    snd <$> foldM escapeWords (charsleft, str) wordfilter
  where escapeWord word (charsleft, pas, as) =
            case findWithSurroundingsLE word as of
                Just (f, v, s) -> do
                    (charsused, a) <- escapeOneWord charsleft v
                    escapeWord word (charsleft - charsused, a:f:pas, s)
                Nothing -> return $ (charsleft, pas, as)
        escapeWords (charsleft, str) word = do
            (chars, pas, as) <- escapeWord word (charsleft, [], str)
            return (chars, concat (reverse pas) ++ as)

-- | Use various methods to mutilate a string
escape :: MonadChoice m => Int -> [String] -> String -> m String
escape charlimit wordfilter str = do
    let max_invs = charlimit - length str
    garbled_str <- randomizeLang str
    cut_str <- if max_invs > 0
                then randomizeInvisible max_invs garbled_str
                else return garbled_str
    mutilated_str <- escapeWordfilter (charlimit - length cut_str) wordfilter cut_str
    return mutilated_str
