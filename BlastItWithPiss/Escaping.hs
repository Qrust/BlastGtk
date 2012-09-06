{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module BlastItWithPiss.Escaping where
import Import
import Control.Monad.Random

-- FIXME code here is a mess.

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
    ,"нульч"
    ,"Нульч"
    ,"1chan.ru"
    ,"0chan.ru"
    ,"моча"
    ,"Моча"
    ,"моче"
    ,"Моче"
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
    ,"@conference.jabber"
    ]
-- FIXME "Fatal error: word \"" ++ str ++ "\" has no characters that can be substituted by equivalents."

-- | List of invisble symbols
invisibleSymbols :: [Char]
invisibleSymbols = "\8290\8291\8289\8288\8203\8206"

zalgo :: [Char]
zalgo = []

-- | Simillar letters in Cyrillic and Latin.
simillars :: [(Char, Char)]
simillars =
    [('a', 'а')
    ,('A', 'А')
    ,('B', 'В')
    ,('c', 'с')
    ,('C', 'С')
    ,('e', 'е')
    ,('E', 'Е')
    ,('H', 'Н')
    ,('i', 'і')
    ,('I', 'І')
    ,('K', 'К')
    ,('M', 'М')
    ,('o', 'о')
    ,('O', 'O')
    ,('p', 'р')
    ,('P', 'Р')
    ,('T', 'Т')
    ,('x', 'х')
    ,('X', 'Х')
    ]

findAlternative :: Char -> Maybe Char
findAlternative c = findMap aux simillars
  where aux (a, b) | c==a = Just b
                   | c==b = Just a
                   | otherwise = Nothing

-- | Randomly substitute letters by simillar looking in different alphabet
randomizeLang :: String -> IO String
randomizeLang str =
    forM str $ \c ->
        case findAlternative c of
            Just a -> ifM getRandom (return a) (return c) --depending on random we either substitute char, or leave it be.
            _ -> return c

insertAfterIxs :: [(Int, a)] -> [a] -> [a]
insertAfterIxs ixs as = case foldl' aux (0, [], as) ixs of
                            (_, pas, as) -> concat (reverse pas) ++ as
  where aux (cur, pas, as) (ix, a) =
            case splitAt (ix-cur) as of
                (f, s) -> (ix, [a]:f:pas, s)

getIxs :: [Int] -> [a] -> [a]
getIxs is as = map (as!!) is --dangerous

-- | Intersperse string in random places with random amount of random invisible characters
randomizeInvisible :: Int -> String -> IO String
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

randomizeOneCharLang :: String -> IO String
randomizeOneCharLang str = do
    let ixs = findIndices (isJust . findAlternative) str
    if null ixs
        then return $ error $ "Fatal error: word \"" ++ str ++ "\" has no characters that can be substituted by equivalents."
        else do (f, (v:s)) <- (`splitAt` str)  . (ixs!!) <$> getRandomR (0, length ixs - 1)
                return $ f ++ [fromJust (findAlternative v)] ++ s

escapeOneWord :: Int -> String -> IO (Int, String)
escapeOneWord charsleft str = do
    cut_str <- randomizeInvisible charsleft str
    if str==cut_str
        then (,) 0 <$> randomizeOneCharLang str
        else do let charsused = length cut_str - length str
                ifM getRandom
                    ((,) charsused <$> randomizeOneCharLang cut_str)
                    (return (charsused, cut_str))

escapeWordfilter :: Int -> [String] -> String -> IO String
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

-- | Use various methods to mutilate string
escape :: Int -> [String] -> String -> IO String
escape charlimit wordfilter str = do
    let max_invs = charlimit - length str
    garbled_str <- randomizeLang str
    cut_str <- if max_invs > 0
                then randomizeInvisible max_invs garbled_str
                else return garbled_str
    mutilated_str <- escapeWordfilter (charlimit - length cut_str) wordfilter cut_str
    return mutilated_str
