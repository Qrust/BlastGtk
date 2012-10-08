module BlastItWithPiss.Board
    (cloudflareRecaptchaKey
    ,Board(..)
    ,readBoard
    ,renderBoard

    -- * Ssach
    ,ssachAdaptivity
    ,ssachBoardsSortedByPostRate
    ,ssachLastRecordedWakabaplAndFields
    ,allSsachBoards
    ,ssach
    ,ssachBoard
    ,ssachThread
    ,ssachPage
    ,ssachRecaptchaKey
    ,ssachLengthLimit
    ,ssachThreadTimeout
    ,ssachPostTimeout

    -- * Hoptoparasha
    ,hoptoparasha
    ,hoptoparashaBoard
    ,hoptoparashaThread
    ,hoptoparashaPage
    ) where
import Import
import BlastItWithPiss.MultipartFormData

-- | Sosach boards.
data Board = A
           | AA
           | ABU
           | APP
           | ASYLUM
           | AU
           | B
           | BB
           | BG
           | BI
           | BIZ
           | BO
           | C
           | CG
           | D
           | DE
           | DEV
           | DI
           | DIY
           | EM
           | EW
           | F
           | FA
           | FAG
           | FD
           | FG
           | FIZ
           | FL
           | FUR
           | G
           | GA
           | GB
           | GD
           | H
           | HI
           | HO
           | HW
           | I
           | INT
           | JA
           | LS
           | MA
           | MC
           | MDK
           | ME
           | MG
           | MLP
           | MMO
           | MO
           | MU
           | MUS
           | NE
           | O
           | P
           | PA
           | PER
           | PO
           | PR
           | PSY
           | R
           | RA
           | RE
           | RF
           | RM
           | S
           | SCI
           | SEX
           | SF
           | SN
           | SOC
           | SP
           | SPC
           | T
           | TD
           | TES
           | TO
           | TV
           | UN
           | VG
           | VN
           | W
           | WH
           | WM
           | WP
           | WR
        -- temporary boards
           | PVC
           | TRV
           | IZD
           | WEB
           | HH
           | DOM
           | FTB
           | DR
    deriving (Eq, Read, Show, Enum, Bounded, Ord)

instance NFData Board

allSsachBoards :: [Board]
allSsachBoards = [minBound..maxBound]

-- | See "BlastItWithPiss.sortSsachBoardsByPopularity".
-- TODO Last update: Fri, 05 Oct 2012 12:57:17 +0300
-- TODO update silently in app background
ssachBoardsSortedByPostRate :: [(Board, Int)]
ssachBoardsSortedByPostRate =
    [(B,1508)
    ,(VG,236)
    ,(PO,170)
    ,(MMO,169)
    ,(MLP,110)
    ,(AU,69)
    ,(T,59)
    ,(SOC,43)
    ,(RF,40)
    ,(SEX,36)
    ,(A,33)
    ,(TV,30)
    ,(S,30)
    ,(MU,29)
    ,(DEV,27)
    ,(HW,24)
    ,(FIZ,24)
    ,(WM,21)
    ,(SCI,18)
    ,(G,17)
    ,(PSY,16)
    ,(RA,14)
    ,(BB,14)
    ,(PR,13)
    ,(P,12)
    ,(MC,12)
    ,(BIZ,12)
    ,(RE,11)
    ,(SF,10)
    ,(DI,8)
    ,(UN,7)
    ,(MO,7)
    ,(MA,7)
    ,(FD,7)
    ,(BO,7)
    ,(MUS,6)
    ,(ME,6)
    ,(FL,6)
    ,(D,6)
    ,(WR,5)
    ,(WH,5)
    ,(TO,5)
    ,(PER,5)
    ,(MG,5)
    ,(GD,5)
    ,(FUR,5)
    ,(DR,5)
    ,(FA,5)
    ,(FTB,4)
    ,(W,4)
    ,(R,4)
    ,(PA,4)
    ,(GA,4)
    ,(EM,4)
    ,(CG,4)
    ,(BI,4)
    ,(TES,3)
    ,(SPC,3)
    ,(I,3)
    ,(H,3)
    ,(FAG,3)
    ,(EW,3)
    ,(C,3)
    ,(DOM,2)
    ,(HH,2)
    ,(WEB,2)
    ,(TRV,2)
    ,(PVC,2)
    ,(SP,2)
    ,(RM,2)
    ,(NE,2)
    ,(MDK,2)
    ,(INT,2)
    ,(FG,2)
    ,(DIY,2)
    ,(BG,2)
    ,(APP,2)
    ,(IZD,1)
    ,(WP,1)
    ,(VN,1)
    ,(TD,1)
    ,(SN,1)
    ,(O,1)
    ,(LS,1)
    ,(JA,1)
    ,(HO,1)
    ,(HI,1)
    ,(GB,1)
    ,(DE,1)
    ,(ASYLUM,1)
    ,(ABU,1)
    ,(AA,1)
    ,(F,0)
    ]

readBoard :: String -> Maybe Board
readBoard x | head x == '/' && last x == '/' = readMay $ map toUpper $ init $ tail x
            | otherwise = Nothing

renderBoard :: (Monoid a, IsString a) => Board -> a
renderBoard b = "/" <> fromString (map toLower $ show b) <> "/"

ssach :: IsString a => a
ssach = "http://2ch.so"

ssachBoard :: (Monoid a, IsString a) => Board -> a
ssachBoard b = ssach <> renderBoard b

ssachThread :: (Monoid a, IsString a) => Board -> Maybe Int -> a
ssachThread b Nothing = ssachBoard b
ssachThread b (Just t) = ssachBoard b <> "res/" <> show t <> ".html"

ssachPage :: (Monoid a, IsString a) => Board -> Int -> a
ssachPage b 0 = ssachBoard b
ssachPage b i = ssachBoard b <> show i <> ".html"

ssachRecaptchaKey :: String
ssachRecaptchaKey = "6LdOEMMSAAAAAIGhmYodlkflEb2C-xgPjyATLnxx"

ssachLengthLimit :: Num a => a
ssachLengthLimit = 7168 -- max number of cyrillic characters, stupid sosach counts
                        -- bytes instead of unicode chars.

ssachThreadTimeout :: Num a => Board -> a
ssachThreadTimeout _ = 30 * 60
ssachPostTimeout :: Num a => Board -> a
-- TODO update later, when they disable adaptivity
ssachPostTimeout _ = if ssachAdaptivity then 10 else 0

ssachAdaptivity :: Bool
-- TODO update later, when they disable adaptivity
ssachAdaptivity = True

ssachLastRecordedWakabaplAndFields :: String -> (String, [Field])
ssachLastRecordedWakabaplAndFields hostAndBoard =
    (hostAndBoard ++ "wakaba.pl",
        [field "task" "\209\128\208\190st"
        ,field "name" ""
        ,field "link" ""
        ,field "akane" ""
        ,field "sage" ""
        ,field "submit" "\208\158\209\130\208\191\209\128\208\176\208\178\208\184\209\130\209\140"
        ,field "video" ""
        ])

hoptoparasha :: IsString a => a
hoptoparasha = "http://hoptach.uni.me"

hoptoparashaBoard :: (Monoid a, IsString a) => Board -> a
hoptoparashaBoard b = hoptoparasha <> renderBoard b

hoptoparashaThread :: (Monoid a, IsString a) => Board -> Maybe Int -> a
hoptoparashaThread b Nothing = hoptoparashaBoard b
hoptoparashaThread b (Just t) = hoptoparashaBoard b <> "res/" <> show t <> ".html"

hoptoparashaPage :: (Monoid a, IsString a) => Board -> Int -> a
hoptoparashaPage b 0 = hoptoparashaBoard b
hoptoparashaPage b i = hoptoparashaBoard b <> show i <> ".html"

-- | This should be put somewhere else
cloudflareRecaptchaKey :: String
cloudflareRecaptchaKey = "6LeT6gcAAAAAAAZ_yDmTMqPH57dJQZdQcu6VFqog"

-- TODO add 2chnu
