module BlastItWithPiss.Board
    (Board(..)
    ,readBoard
    ,renderBoard

    -- * Ssach
    ,ssachBoardsSortedByPostRate
    ,ssachLastRecordedFields
    ,allSsachBoards
    ,ssach
    ,ssachBoard
    ,ssachThread
    ,ssachPage
    ,ssachPostUrl
    ,ssachLengthLimit
    ,ssachThreadTimeout
    ,ssachPostTimeout
    ,ssachBumpLimit

    -- * Hoptoparasha
    ,hoptoparasha
    ,hoptoparashaBoard
    ,hoptoparashaThread
    ,hoptoparashaPage
    ) where
import Import
import Network.HTTP.Conduit.MultipartFormData

-- | Sosach boards.
data Board = A
           | AA
           | ABU
           | APP -- temp/hid
           | ASYLUM -- temp/hid
           | AU
           | B
--           | BB
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
           | DOM -- temp/hid
--           | DR -- temp/hid
           | EM
           | EW
           | F
           | FA
           | FAG
           | FD
           | FG
           | FIZ
           | FL
           | FTB -- temp/hid
           | FUR
           | G
           | GA
           | GB
           | GD
           | GIF -- temp/hid
           | H
           | HH -- temp/hid
           | HI
           | HO
           | HW
           | I
           | INT
           | IZD -- temp/hid
           | JA
--           | LS
           | MA
           | MC
           | MDK
           | ME
           | MG
           | MLP
           | MMO
           | MO
           | MOV -- temp/hid
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
           | PVC -- temp/hid
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
           | TRV -- temp/hid
           | TV
           | UN
           | VG
           | VN
           | W
           | WEB -- temp/hid
           | WH
           | WM
           | WP
           | WR
    deriving (Eq, Read, Show, Enum, Bounded, Ord)

instance NFData Board

allSsachBoards :: [Board]
allSsachBoards = [minBound..maxBound]

-- | See "BlastItWithPiss.sortSsachBoardsByPopularity".
-- TODO Last update: Mon, 29 Oct 2012 22:49:11 +0200
-- TODO update silently in app background
ssachBoardsSortedByPostRate :: [(Board, Int)]
ssachBoardsSortedByPostRate =
    [(B,2067)
    ,(VG,392)
    ,(PO,188)
    ,(MMO,112)
    ,(T,106)
    ,(DEV,95)
    ,(A,88)
    ,(AU,87)
    ,(MLP,86)
    ,(ASYLUM,83)
    ,(SEX,71)
    ,(MU,61)
    ,(RF,59)
    ,(FA,55)
--    ,(BB,45)
    ,(SOC,39)
    ,(GA,39)
    ,(MOV,34)
    ,(TV,26)
    ,(FAG,26)
    ,(ME,25)
    ,(GIF,23)
    ,(RE,22)
    ,(FIZ,22)
    ,(ABU,22)
    ,(WM,21)
    ,(G,21)
    ,(HW,20)
    ,(CG,20)
    ,(FTB,16)
    ,(D,16)
    ,(S,13)
    ,(BI,13)
    ,(PR,10)
--    ,(DR,10)
    ,(P,9)
    ,(MA,9)
    ,(DI,9)
    ,(VN,7)
    ,(MC,7)
    ,(GD,7)
    ,(PSY,6)
    ,(PER,6)
    ,(MUS,6)
    ,(HI,6)
    ,(WH,5)
    ,(BO,5)
    ,(HO,4)
    ,(H,4)
    ,(TES,3)
    ,(SP,3)
    ,(SN,3)
    ,(SCI,3)
    ,(FG,3)
    ,(EW,3)
    ,(DE,3)
    ,(BIZ,3)
    ,(BG,3)
    ,(WEB,2)
    ,(TO,2)
    ,(RM,2)
    ,(RA,2)
    ,(R,2)
    ,(O,2)
    ,(MO,2)
    ,(MDK,2)
    ,(JA,2)
    ,(INT,2)
    ,(FL,2)
    ,(DIY,2)
    ,(C,2)
    ,(APP,2)
    ,(WR,1)
    ,(WP,1)
    ,(W,1)
    ,(UN,1)
    ,(TRV,1)
    ,(TD,1)
    ,(SPC,1)
    ,(SF,1)
    ,(PVC,1)
    ,(PA,1)
    ,(NE,1)
    ,(MG,1)
--    ,(LS,1)
    ,(I,1)
    ,(HH,1)
    ,(GB,1)
    ,(FUR,1)
    ,(FD,1)
    ,(EM,1)
    ,(DOM,1)
    ,(AA,1)
    ,(IZD,0)
    ,(F,0)
    ]

readBoard :: String -> Maybe Board
readBoard ('/':x@(_:_))
    | last x == '/' = readMay $ map toUpper $ init x
    | otherwise = Nothing
readBoard _ = Nothing

renderBoard :: (Monoid a, IsString a) => Board -> a
renderBoard b = "/" <> fromString (map toLower $ show b) <> "/"

ssach :: IsString a => a
--ssach = "http://2ch.so"
-- TODO update later, when they get their domain back
ssach = "http://2ch.hk"

ssachBoard :: (Monoid a, IsString a) => Board -> a
ssachBoard b = ssach <> renderBoard b

ssachThread :: (Monoid a, IsString a) => Board -> Maybe Int -> a
ssachThread b Nothing = ssachBoard b
ssachThread b (Just t) = ssachBoard b <> "res/" <> show t <> ".html"

ssachPage :: (Monoid a, IsString a) => Board -> Int -> a
ssachPage b 0 = ssachBoard b <> "wakaba.html"
ssachPage b i = ssachBoard b <> show i <> ".html"

ssachPostUrl :: (Monoid a, IsString a) => Board -> Maybe Int -> a
ssachPostUrl b _ = ssachBoard b <> "wakaba.pl"

ssachLengthLimit :: Num a => a
ssachLengthLimit = 7168 -- max number of cyrillic characters, stupid sosach counts
                        -- bytes instead of unicode chars.

ssachThreadTimeout :: Num a => Board -> a
ssachThreadTimeout _ = 30 * 60
ssachPostTimeout :: Num a => Board -> a
ssachPostTimeout _ = 10

ssachBumpLimit :: Num a => Board -> a
ssachBumpLimit B = 513
ssachBumpLimit _ = 1000 -- TODO ssachbumplimit

ssachLastRecordedFields :: (Monad m, Monad m') => Board -> [Part m m']
ssachLastRecordedFields board =
    [partBS "task" "\209\128\208\190st"
    ,partBS "name" ""
    ,partBS "link" ""
    ,partBS "akane" ""
    ,partBS "sage" ""
    ,partBS "submit" "\208\158\209\130\208\191\209\128\208\176\208\178\208\184\209\130\209\140"
    ,partBS "video" ""
    ] ++
    [partBS "anon_icon" "-1" | board==PO]

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

-- TODO add 2chnu
