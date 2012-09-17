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
    deriving (Eq, Read, Show, Enum, Bounded, Ord)

allSsachBoards :: [Board]
allSsachBoards = [minBound..maxBound]

-- | See "BlastItWithPiss.sortSsachBoardsByPopularity".
-- TODO Last update: Sat, 15 Sep 2012 23:47:15 +0300
-- TODO update silently in app background
ssachBoardsSortedByPostRate :: [(Board, Int)]
ssachBoardsSortedByPostRate =
    [(B,3757)
    ,(VG,475)
    ,(MLP,258)
    ,(MMO,188)
    ,(PO,171)
    ,(T,138)
    ,(SOC,132)
    ,(RF,111)
    ,(SEX,87)
    ,(A,82)
    ,(FIZ,78)
    ,(AU,60)
    ,(SN,54)
    ,(DIY,40)
    ,(WM,38)
    ,(MA,38)
    ,(MU,36)
    ,(GA,33)
    ,(S,28)
    ,(G,28)
    ,(DEV,25)
    ,(RA,21)
    ,(MC,21)
    ,(D,20)
    ,(TV,19)
    ,(FA,18)
    ,(TES,15)
    ,(PSY,15)
    ,(ME,15)
    ,(FAG,15)
    ,(BB,15)
    ,(FL,14)
    ,(RE,13)
    ,(CG,13)
    ,(BO,13)
    ,(SP,12)
    ,(MUS,12)
    ,(FG,12)
    ,(C,12)
    ,(BI,12)
    ,(PR,11)
    ,(PER,11)
    ,(P,11)
    ,(HW,10)
    ,(EW,10)
    ,(R,8)
    ,(BIZ,8)
    ,(WR,7)
    ,(WH,7)
    ,(W,7)
    ,(UN,7)
    ,(MDK,7)
    ,(EM,7)
    ,(NE,6)
    ,(SCI,5)
    ,(PA,5)
    ,(MG,5)
    ,(HI,5)
    ,(H,5)
    ,(FD,5)
    ,(AA,5)
    ,(VN,4)
    ,(SF,4)
    ,(LS,4)
    ,(DI,4)
    ,(APP,4)
    ,(SPC,3)
    ,(MO,3)
    ,(BG,3)
    ,(RM,2)
    ,(JA,2)
    ,(I,2)
    ,(ASYLUM,2)
    ,(WP,1)
    ,(TO,1)
    ,(TD,1)
    ,(O,1)
    ,(HO,1)
    ,(GD,1)
    ,(FUR,1)
    ,(F,1)
    ,(DE,1)
    ,(INT,0)
    ,(GB,0)
    ,(ABU,0)]

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
ssachPostTimeout _ = 10

hoptoparasha :: IsString a => a
hoptoparasha = "http://hoptach.uni.me"

hoptoparashaBoard :: (Monoid a, IsString a) => Board -> a
hoptoparashaBoard b = hoptoparasha <> renderBoard b

hoptoparashaThread :: (Monoid a, IsString a) => Board -> Int -> a
hoptoparashaThread b t = hoptoparashaBoard b <> "res/" <> show t <> ".html"

hoptoparashaPage :: (Monoid a, IsString a) => Board -> Int -> a
hoptoparashaPage b 0 = hoptoparashaBoard b
hoptoparashaPage b i = hoptoparashaBoard b <> show i <> ".html"

-- TODO add 2chnu
