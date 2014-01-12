module BlastItWithPiss.Escaping
    (
     escape

    ,ssachWordfilter

    ,escapeWordfilter
    ,escapeExceptWordfilter
    ) where
import Import
import BlastItWithPiss.MonadChoice

import Data.Tuple (swap)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- FIXME Кажется за каждый reverse мне светит по ебалу

-- | Words which need escaping
ssachWordfilter :: [String]
ssachWordfilter =
    ["сосач"
    ,"Сосач"
    ,"сомали"
    ,"Сомали"
    ,"тире"
    ,"Тире"
    ,"двач"
    ,"Двач"
    ,"колчан"
    ,"Колчан"
    ,"нульча"
    ,"Нульча"
    ,"0ch"
    ,"1ch"
    ,"2ch"
    ,"2-ch"
    ,"2--ch"
    ,"dvach"
    ,"dva-ch"
    ,"2chru"
    ,"net"
    ,"chan"
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
    ,"Инач"
    ,"инач"
    ,"Инали"
    ,"инали"
    ,"Елизаве"
    ,"елизаве"
    ,"Elizabe"
    ,"Харк"
    ,"харк"
    ,"Харч"
    ,"харч"
    ,"ола"
    ,"Ола"
    ,"очко"
    ,"Очко"
    ,"петух"
    ,"Биборан"
    ,"биборан"
    ]

-- | List of invisble symbols
invisibleSymbols :: [Char]
invisibleSymbols = "\8290\8291\8289\8288\8203\8206\65279\173\8237"
{-
-- | TODO zalgo
zalgo :: [Char]
zalgo = []
-}
-- TODO Math symbols
-- TODO diacretics

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
    ,('ɜ', 'з')
    ,('ʜ', 'н')
    ,('H', 'Н')
    ,('Η', 'Н')
    ,('i', 'і')
    ,('I', 'І')
    ,('Ï', 'Ї')
    ,('K', 'К')
    ,('M', 'М')
    ,('ᴎ', 'и')
    ,('𐑍', 'и')
    ,('o', 'о')
    ,('O', 'O')
    ,('ο', 'о')
    ,('Ο', 'О')
    ,('p', 'р') -- FIXME duplicates, not a Map.
    ,('P', 'Р')
    ,('Ρ', 'Р')
    ,('T', 'Т')
    ,('Τ', 'Т')
    ,('x', 'х')
    ,('X', 'Х')
    ,('϶', 'э')
    ,('ϵ', 'є')
    ,('ᴙ', 'я')
    ]

{-# NOINLINE toCyr #-}
toCyr :: IntMap Char
toCyr = IntMap.fromList $ map (first fromEnum) simillars

{-# NOINLINE fromCyr #-}
fromCyr :: IntMap Char
fromCyr = IntMap.fromList $ map (first fromEnum) $ map swap simillars

-- | Lookup 'simillars'
findAlternative :: Char -> Maybe Char
findAlternative c =
    let !i = fromEnum c
    in IntMap.lookup i fromCyr &
        maybe (IntMap.lookup i toCyr) Just

-- | Randomly substitute letters by simillar looking in different alphabet
randomizeLang :: MonadChoice m => String -> m String
randomizeLang str =
    forM str $ \c ->
        case findAlternative c of
          Just a ->
            -- 50/50, either substitute char, or leave it be.
            ifM getRandom
                (return a)
                (return c)
          _ -> return c

insertAfterIxs :: [(Int, a)] -> [a] -> [a]
insertAfterIxs ixs as' =
    let (_, pas, as) = foldl' aux (0, [], as') ixs
    in concat (reverse pas) ++ as
  where
    aux (!cur, pas, as) (ix, a) =
        let (f, s) =  splitAt (ix - cur) as
        in (ix, [a]:f:pas, s)

getIxs :: [Int] -> [a] -> [a]
getIxs is as = map (as!!) is -- dangerous

-- | Intersperse string in random places with random amount of random invisible
-- characters
randomizeInvisible :: MonadChoice m => Int -> String -> m String
randomizeInvisible max_invs' str = do
-- HACK since each of our invisible chars consists of three unicode code points
--      we divide the limit by three.
-- FIXME symbols should be counted by their UTF8 length in bytes.
    let max_invs = max_invs' `div` 3
    ixlen <- getRandomR (0, max_invs)
    ixs <- sort . take ixlen <$> getRandomRs (0, length str)
    syms <-
        flip getIxs invisibleSymbols
            . take ixlen
                <$> getRandomRs (0, length invisibleSymbols-1)
    return $ insertAfterIxs (zip ixs syms) str

randomizeOneCharLang :: MonadChoice m => String -> m String
randomizeOneCharLang str = do
    let ixs = findIndices (isJust . findAlternative) str
    if null ixs
      then
        return str
      else do
        (f, (v:s)) <- (`splitAt` str) <$> chooseFromList ixs
        return $ f ++ [fromJust (findAlternative v)] ++ s

escapeOneWord :: MonadChoice m => Int -> String -> m (Int, String)
escapeOneWord charsleft str = do
    cut_str <- randomizeInvisible charsleft str
    if str == cut_str -- needs additional escaping
      then
        (,) 0 <$> randomizeOneCharLang str
      else do
        let charsused = length cut_str - length str
        randomBool <- getRandom
        if randomBool
          then
            (,) charsused <$> randomizeOneCharLang cut_str
          else
            return (charsused, cut_str)

escapeExceptWordfilter :: MonadChoice m => Int -> String -> m String
escapeExceptWordfilter charlimit str = do
    let max_invs = charlimit - length str
    garbled_str <- randomizeLang str
    if max_invs > 0
      then
        randomizeInvisible max_invs garbled_str
      else
        return garbled_str

escapeWordfilter :: MonadChoice m => Int -> [String] -> String -> m String
escapeWordfilter _charsleft _wordfilter _str =
    snd <$> foldM escapeWords (_charsleft, _str) _wordfilter
  where
    escapeWord word (charsleft, pas, as) =
        case findWithSurroundingsLE word as of
          Just (f, v, s) -> do
            (charsused, a) <- escapeOneWord charsleft v
            escapeWord word (charsleft - charsused, a:f:pas, s)
          Nothing ->
            return (charsleft, pas, as)

    escapeWords (charsleft, str) word = do
        (chars, pas, as) <- escapeWord word (charsleft, [], str)
        return (chars, concat (reverse pas) ++ as)

-- | Use various methods to mutilate a string. Slow.
escape :: MonadChoice m => Int -> [String] -> String -> m String
escape charlimit wordfilter str = do
    cut_str <-
        escapeExceptWordfilter charlimit str

    mutilated_str <-
        escapeWordfilter (charlimit - length cut_str) wordfilter cut_str

    return mutilated_str
