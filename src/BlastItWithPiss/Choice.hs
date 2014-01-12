-- | kokoko
module BlastItWithPiss.Choice
    (
     Mode(..)
    ,allModes

    ,obligatorySageMode
    ,obligatoryImageMode

    ,chooseMode
    ,chooseThread
    ) where
import Import hiding ((/))

import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.MonadChoice (MonadChoice)
import qualified BlastItWithPiss.MonadChoice as Choose

import Data.Ratio

import qualified Data.Set as S
import qualified Data.Map as M

data Mode
    = SagePopular
    | BumpUnpopular
    | ShitupSticky
    | BumpOld       -- ^ Flip board upside down
    | CreateNew     -- ^ Erase threads by creating new threads
  deriving (Eq, Show, Ord, Enum, Bounded)

{-# NOINLINE allModes #-}
allModes :: [Mode]
allModes = [minBound .. maxBound]

type Strategy = [(Mode, Rational)]

instance NFData Mode

obligatorySageMode :: Mode -> Bool
obligatorySageMode SagePopular = True
obligatorySageMode _ = False

obligatoryImageMode :: Mode -> Bool
obligatoryImageMode CreateNew = True
obligatoryImageMode _ = False

infixr 0 /
(/) :: a -> b -> (a, b)
(/) = (,)

always :: Rational
always = 100000000

-- | Different strategies for different boards.
strategies :: M.Map Board Strategy
strategies =
    M.fromList $
    [SsachVG /
        [SagePopular    / 70
        ,BumpUnpopular  / 10
        ,ShitupSticky   / 50
        ,BumpOld        / 20
        ,CreateNew      / always]
    ,SsachB /
        [SagePopular    / 70
        ,BumpUnpopular  / 20
        ,ShitupSticky   / 50
        ,BumpOld        / 10
        ,CreateNew      / always]
    ,SsachCG /
        [SagePopular    / 40
        ,BumpUnpopular  / 20
        ,ShitupSticky   / 50
        ,BumpOld        / 60
        ,CreateNew      / always]
    ,SsachD /
        [SagePopular    / 50
        ,BumpUnpopular  / 0
        ,ShitupSticky   / 50
        ,BumpOld        / 30
        ,CreateNew      / always]
    ,SsachMLP /
        [SagePopular    / 60
        ,BumpUnpopular  / 10
        ,ShitupSticky   / 100
        ,BumpOld        / 30
        ,CreateNew      / always]
{- FIXME BB REMOVED
    ,SsachBB /
        [SagePopular    / 20
        ,BumpUnpopular  / 20
        ,ShitupSticky   / 40
        ,BumpOld        / 60
        ,CreateNew      / always]
-}
    ,SsachDEV /
        [SagePopular    / 15
        ,BumpUnpopular  / 15
        ,ShitupSticky   / 60
        ,BumpOld        / 50
        ,CreateNew      / always]
    ,SsachS /
        [SagePopular    / 33
        ,BumpUnpopular  / 33
        ,ShitupSticky   / 100
        ,BumpOld        / 33
        ,CreateNew      / always]
    ,SsachMMO /
        [SagePopular    / 50
        ,BumpUnpopular  / 40
        ,ShitupSticky   / 100
        ,BumpOld        / 10
        ,CreateNew      / always]
    ,SsachA /
        [SagePopular    / 70
        ,BumpUnpopular  / 20
        ,ShitupSticky   / 40
        ,BumpOld        / 10
        ,CreateNew      / always]
    ,SsachPO /
        [SagePopular    / 35
        ,BumpUnpopular  / 5
        ,ShitupSticky   / 40
        ,BumpOld        / 20
        ,CreateNew      / always]
    ,SsachSOC /
        [SagePopular    / 33
        ,BumpUnpopular  / 15
        ,ShitupSticky   / 33
        ,BumpOld        / 18
        ,CreateNew      / always]
    ,SsachSEX /
        [SagePopular    / 35
        ,BumpUnpopular  / 45
        ,ShitupSticky   / 50
        ,BumpOld        / 20
        ,CreateNew      / always]
    ,SsachTV /
        [SagePopular    / 40
        ,BumpUnpopular  / 20
        ,ShitupSticky   / 60
        ,BumpOld        / 20
        ,CreateNew      / always]
    ,SsachFA /
        [SagePopular    / 33
        ,BumpUnpopular  / 15
        ,ShitupSticky   / 15
        ,BumpOld        / 37
        ,CreateNew      / always]
    ,SsachRF /
        [SagePopular    / 33
        ,BumpUnpopular  / 33
        ,ShitupSticky   / 60
        ,BumpOld        / 34
        ,CreateNew      / always]
    ,SsachD /
        [SagePopular    / 10
        ,BumpUnpopular  / 20
        ,ShitupSticky   / 30
        ,BumpOld        / 40
        ,CreateNew      / always]
    ,SsachABU /
        [SagePopular    / 100
        ,BumpUnpopular  / 25
        ,ShitupSticky   / 100
        ,BumpOld        / 50
        ,CreateNew      / 0]
    ]

-- | For boards not listed in "strategies"
defaultStrategy :: Strategy
defaultStrategy =
    [SagePopular    / 10
    ,BumpUnpopular  / 15
    ,ShitupSticky   / 20
    ,BumpOld        / 25
    ,CreateNew      / always]

unlocked :: Thread -> Bool
unlocked = not . locked

notSticky :: Thread -> Bool
notSticky = not . pinned

bumpable :: Board -> Thread -> Bool
bumpable board t = postcount t < ssachBumpLimit board

unlockedUnpinnedBump :: Board -> Thread -> Bool
unlockedUnpinnedBump board t = unlocked t && notSticky t && bumpable board t

unlockedSticky :: Thread -> Bool
unlockedSticky t = not (locked t) && pinned t

newThread :: Thread -> Bool
newThread t = postcount t <= 5

popularThread :: Thread -> Bool
popularThread t = postcount t >= 100

tooFast :: Int -> Bool
tooFast s = s >= 200

-- | We take a look at the front page
-- and adjust our strategy depending on what we see.
-- (whether there are more empty threads or official threads.
adjustStrategy :: Strategy -> Bool -> Page -> Strategy
adjustStrategy strategy canmakethread Page{ threads, speed }
    -- >kokoko
    | not (null threads)
    = let
        len :: Integer
        !len  = fromIntegral (length threads)

        new, vpop :: Rational
        !new  = fromIntegral (length $ filter newThread threads) % len
        !vpop = fromIntegral (length $ filter popularThread threads) % len

        !nps = if tooFast speed
              then [CreateNew, BumpUnpopular]
              else [BumpOld, ShitupSticky]
        !vps = if tooFast speed
            then [SagePopular, ShitupSticky]
            else [BumpOld, CreateNew]

        aux (x, r)
          | let y = r * if' (x `elem` nps) new 0
                z = r * if' (x `elem` vps) vpop 0
          = (x, r + y + z)
     in map aux (filter goodStrategy strategy)

    | otherwise
     = filter goodStrategy strategy -- abort
  where
    goodStrategy (md, _) =
        notElem md $
            [CreateNew | not canmakethread] ++
            [ShitupSticky | not (any unlockedSticky threads)]

chooseMode
    :: MonadChoice m
    => Board -> Bool -> Page -> S.Set Mode -> m (Maybe Mode)
chooseMode board canmakethread page allowed =
    case
      filter ((`S.member` allowed) . fst) $
        adjustStrategy
          (fromMaybe defaultStrategy (M.lookup board strategies))
          canmakethread
          page of
      [] -> return Nothing
      a  -> Just <$> Choose.fromList a

chooseThread'
    :: MonadChoice m
    => Board -> Mode -> [Thread] -> m (Maybe Int)
chooseThread' _ CreateNew _ =
    error "chooseThread': WTF, chooseThread with CreateNew, this should never happen"
chooseThread' board mode threads
    | null threads = error "chooseThread': No threads to choose from"
    -- >kokoko
    | thrds' <-
        let filtered = if mode == ShitupSticky
                then filter (unlockedSticky) threads
                else filter (unlockedUnpinnedBump board) threads
        in if null filtered
          then
            -- any thread we can post in
            filter unlocked threads
          else filtered
    , thrds <-
        if mode /= ShitupSticky
          -- add the possibility of failure
          -- in that case we advance to the next/previous page
          then (Thread (-1) False False 50 [] : thrds')
          else thrds'
    , inv <-
        -- these modes give more weight to unpopular threads
        if mode == BumpUnpopular || mode == BumpOld
          then let max' = fromIntegral $ maximum $ map postcount thrds
            in (\w -> max' - w)
          else id
     = justIf (> (-1)) <$> Choose.fromList
            (map (threadId &&& inv . fromIntegral . postcount) thrds)
  where
    justIf _pred a = if _pred a then Just a else Nothing

chooseThread
    :: MonadChoice m
    => Board -> Mode -> (Int -> m Page) -> Page -> m (Maybe Int, Page)
chooseThread _ CreateNew _ p0 =
    return (Nothing, p0)
chooseThread board mode getPage p0
    | iterpages <-
        if mode == BumpOld
          then
            -- traverse from end
            map getPage (reverse [pageId p0 .. lastpage p0])
          else
            -- traverse from beginning while not redownloading first page
            return p0 : map getPage (tailSafe [pageId p0 .. lastpage p0])
    = untilJust $ (`findMapM` cycle iterpages) $ \getPage' -> do
        page <- getPage'
        maybe Nothing (\x -> Just (Just x, page)) <$>
            chooseThread' board mode (threads page)
