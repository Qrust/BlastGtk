module BlastItWithPiss.Choice where
import Import
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import Data.Ratio
import BlastItWithPiss.MonadChoice
import qualified Data.Map as M

data Mode = SagePopular
          | BumpUnpopular
          | ShitupSticky
          | BumpOld -- ^ Try to turn board upside down
          | CreateNew -- ^ Try to erase threads by creating new threads
    deriving (Eq, Show, Ord, Enum, Bounded)

type Strategy = [(Mode, Rational)]

sageMode :: Mode -> Bool
sageMode SagePopular = True
sageMode _ = False

-- | Different strategies for different boards.
strategies :: M.Map Board Strategy
strategies =
    let infixr 0 /
        (/) = (,)
        always = 100000000
    in M.fromList $
        [VG /    -- everyone sits in generals, nobody cares about the front page
            [SagePopular    / 50
            ,BumpUnpopular  / 5
            ,ShitupSticky   / 20
            ,BumpOld        / 5
            ,CreateNew      / 20]
        ,B /
            [SagePopular / 60
            ,BumpUnpopular / 40
            ,ShitupSticky / 100
            ,BumpOld / 0
            ,CreateNew / 150]
        ,CG /
            [SagePopular / 40
            ,BumpUnpopular / 20
            ,ShitupSticky / 100
            ,BumpOld / 60
            ,CreateNew / always]
        ,D /
            [SagePopular / 20
            ,BumpUnpopular / 0
            ,ShitupSticky / 70
            ,BumpOld / 10
            ,CreateNew / always]
        ,MLP /
            [SagePopular / 60
            ,BumpUnpopular / 20
            ,ShitupSticky / 100
            ,BumpOld / 20
            ,CreateNew / always]
        ,BB /
            [SagePopular / 20
            ,BumpUnpopular / 40
            ,ShitupSticky / 10
            ,BumpOld / 30
            ,CreateNew / always]
        ,DEV /
            [SagePopular / 15
            ,BumpUnpopular / 15
            ,ShitupSticky / 20
            ,BumpOld / 50
            ,CreateNew / always]
        ,S /
            [SagePopular / 33
            ,BumpUnpopular / 33
            ,ShitupSticky / 100
            ,BumpOld / 33
            ,CreateNew / always]
        , MMO /
            [SagePopular / 50
            ,BumpUnpopular / 40
            ,ShitupSticky / 100
            ,BumpOld / 10
            ,CreateNew / always]
        ,A /
            [SagePopular / 50
            ,BumpUnpopular / 20
            ,ShitupSticky / 20
            ,BumpOld / 10
            ,CreateNew / always]
        ,PO /
            [SagePopular / 20
            ,BumpUnpopular / 10
            ,ShitupSticky / 40
            ,BumpOld / 30
            ,CreateNew / always]
        ,SOC /
            [SagePopular / 33
            ,BumpUnpopular / 15
            ,ShitupSticky / 33
            ,BumpOld / 18
            ,CreateNew / always]
        ,SEX /
            [SagePopular / 35
            ,BumpUnpopular / 45
            ,ShitupSticky / 100
            ,BumpOld / 20
            ,CreateNew / always]
        ,TV /
            [SagePopular / 40
            ,BumpUnpopular / 20
            ,ShitupSticky / 20
            ,BumpOld / 20
            ,CreateNew / always]
        ,FA /
            [SagePopular / 33
            ,BumpUnpopular / 15
            ,ShitupSticky / 15
            ,BumpOld / 37
            ,CreateNew / always]
        ,RF /
            [SagePopular / 33
            ,BumpUnpopular / 33
            ,ShitupSticky / 100
            ,BumpOld / 33
            ,CreateNew / always]
        ,D /
            [SagePopular / 10
            ,BumpUnpopular / 20
            ,ShitupSticky / 30
            ,BumpOld / 40
            ,CreateNew / always]
        ,ABU /
            [SagePopular / 0
            ,BumpUnpopular / 0
            ,ShitupSticky / 100
            ,BumpOld / 50
            ,CreateNew / 0]
        ]

{- disregard for now
bumplimits :: Board -> Int
bumplimits = undefined
-}

-- | For boards not listed in "strategies"
defaultStrategy :: Strategy
defaultStrategy =
    let infixr 0 /
        (/) = (,)
        always = 1000000000
    in [SagePopular    / 10
       ,BumpUnpopular  / 30
       ,ShitupSticky   / 20
       ,BumpOld        / 35
       ,CreateNew      / always]

unlockedSticky :: Thread -> Bool
unlockedSticky Thread{..} = not locked && pinned

newThread :: Thread -> Bool
newThread t = postcount t <= 10

veryPopularThread :: Thread -> Bool
veryPopularThread t = postcount t >= 150

tooFast :: Int -> Bool
tooFast s = s >= 200

-- | We take a look at the front page, and adjust our strategy depending on what we see.
adjustStrategy :: Strategy -> Bool -> Page -> Strategy
adjustStrategy strategy canmakethread Page{..}
    --inb4 >kokoko
    | badst <- (\(st, _) -> any ($ st) $
                                [(==CreateNew) | not canmakethread]
                              ++[(==ShitupSticky) | not $ any unlockedSticky threads])
    , len <- fromIntegral $ length threads
    , new <- fromIntegral (length $ filter newThread threads) % len
    , vpop <- fromIntegral (length $ filter veryPopularThread threads) % len
    , nps <- if tooFast speed
                then [CreateNew, BumpUnpopular]
                else [BumpOld, ShitupSticky]
    , vps <- if tooFast speed
                then [SagePopular, ShitupSticky]
                else [BumpOld, CreateNew]
    , aux <- (\(x, r) -> let y = r * (if' (x `elem` nps) new 0)
                             z = r * (if' (x `elem` vps) vpop 0)
                        in (x, r + y + z))
    = map aux $ filter (not . badst) strategy

chooseStrategy :: Board -> Bool -> Page -> Strategy
chooseStrategy board =
    adjustStrategy (fromMaybe defaultStrategy (M.lookup board strategies))

chooseModeStrategy :: MonadRandom m => Strategy -> m Mode
chooseModeStrategy = fromList

chooseMode :: MonadRandom m => Board -> Bool -> Page -> m Mode
chooseMode a b c = chooseModeStrategy $ chooseStrategy a b c

chooseThread' :: MonadChoice m => Mode -> Page -> m (Maybe Int)
chooseThread' CreateNew Page{..} = do
    liftIO $ putStrLn "WTF, this should never happen. Whatever..."
    return Nothing
chooseThread' mode Page{..}
    --inb4 >kokoko
    | thrds <- if mode == ShitupSticky
                then filter unlockedSticky threads
                else threads
    , add <- if mode /= ShitupSticky
                then ((0, 50) :)
                else id
    , inv <- if mode == BumpUnpopular || mode == BumpOld
                then ((fromIntegral $ maximum $ map postcount thrds) -)
                else id
    = justIf (/= 0) <$> fromList (add $
                map (tupapp threadId (inv . fromIntegral . postcount)) thrds)

chooseThread :: MonadChoice m => Mode -> (Int -> m Page) -> Page -> m (Maybe Int)
chooseThread mode getPage p0 = do
    if mode==CreateNew
        then return Nothing
        else let iterpages = if mode==BumpOld
                                then map getPage $ reverse [pageId p0 .. lastpage p0]
                                else (return p0 :) $ tailSafe $
                                      map getPage [pageId p0 .. lastpage p0]
             in Just <$> untilJust (findMapM (chooseThread' mode =<<) iterpages)
