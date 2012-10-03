module BlastItWithPiss.Board where
import Import

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
    deriving (Eq, Read, Show, Enum, Bounded, Ord)

instance NFData Board

allSsachBoards :: [Board]
allSsachBoards = [minBound..maxBound]

-- | See "BlastItWithPiss.sortSsachBoardsByPopularity".
-- TODO Last update: Wed, 19 Sep 2012 16:08:43 +0300
-- TODO update silently in app background
ssachBoardsSortedByPostRate :: [(Board, Int)]
ssachBoardsSortedByPostRate =
    [(B,2523)
    ,(VG,475)
    ,(MMO,296)
    ,(PO,207)
    ,(MLP,145)
    ,(SEX,120)
    ,(SOC,116)
    ,(MU,116)
    ,(AU,90)
    ,(A,73)
    ,(RF,72)
    ,(T,62)
    ,(TV,45)
    ,(MA,45)
    ,(BB,42)
    ,(D,41)
    ,(DEV,39)
    ,(FIZ,37)
    ,(S,30)
    ,(FA,30)
    ,(CG,30)
    ,(HW,29)
    ,(BO,27)
    ,(BIZ,27)
    ,(G,24)
    ,(WM,20)
    ,(PR,19)
    ,(FTB,18)
    ,(WH,18)
    ,(ME,18)
    ,(APP,18)
    ,(RE,17)
    ,(MUS,17)
    ,(P,16)
    ,(BI,16)
    ,(PSY,15)
    ,(FD,15)
    ,(SCI,14)
    ,(MC,14)
    ,(GA,14)
    ,(MG,12)
    ,(DI,12)
    ,(EM,11)
    ,(PER,10)
    ,(WR,9)
    ,(MDK,9)
    ,(I,8)
    ,(EW,8)
    ,(VN,7)
    ,(UN,7)
    ,(W,6)
    ,(SN,6)
    ,(FUR,6)
    ,(FL,6)
    ,(FG,6)
    ,(FAG,6)
    ,(WEB,5)
    ,(SPC,5)
    ,(SP,5)
    ,(MO,5)
    ,(C,5)
    ,(BG,5)
    ,(RA,4)
    ,(NE,4)
    ,(GD,4)
    ,(DOM,3)
    ,(TES,3)
    ,(H,3)
    ,(DIY,3)
    ,(HH,2)
    ,(PVC,2)
    ,(WP,2)
    ,(TO,2)
    ,(R,2)
    ,(PA,2)
    ,(HO,2)
    ,(HI,2)
    ,(ASYLUM,2)
    ,(IZD,1)
    ,(TD,1)
    ,(JA,1)
    ,(INT,1)
    ,(AA,1)
    ,(TRV,0)
    ,(SF,0)
    ,(RM,0)
    ,(O,0)
    ,(LS,0)
    ,(GB,0)
    ,(F,0)
    ,(DE,0)
    ,(ABU,0)
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

ssachThread :: (Monoid a, IsString a) => Board -> Int -> a
ssachThread b t = ssachBoard b <> "res/" <> show t <> ".html"

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
-- TODO update later, when they reintroduce adaptivity
ssachPostTimeout _ = if ssachAdaptivity then 10 else 0

ssachAdaptivity :: Bool
-- TODO update later, when they reintroduce adaptivity
ssachAdaptivity = False

hoptoparasha :: IsString a => a
hoptoparasha = "http://hoptach.uni.me"

hoptoparashaBoard :: (Monoid a, IsString a) => Board -> a
hoptoparashaBoard b = hoptoparasha <> renderBoard b

hoptoparashaThread :: (Monoid a, IsString a) => Board -> Int -> a
hoptoparashaThread b t = hoptoparashaBoard b <> "res/" <> show t <> ".html"

hoptoparashaPage :: (Monoid a, IsString a) => Board -> Int -> a
hoptoparashaPage b 0 = hoptoparashaBoard b
hoptoparashaPage b i = hoptoparashaBoard b <> show i <> ".html"

cloudflareRecaptchaKey :: String
cloudflareRecaptchaKey = "6LeT6gcAAAAAAAZ_yDmTMqPH57dJQZdQcu6VFqog"

-- TODO add 2chnu
