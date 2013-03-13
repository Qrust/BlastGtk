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

-- | Ssach boards.
data Board
    = SsachA
    | SsachAA
    | SsachABU
    | SsachAPP -- temp/hid
    | SsachASYLUM -- temp/hid
    | SsachAU
    | SsachB
--  | SsachBB
    | SsachBG
    | SsachBI
    | SsachBIZ
    | SsachBO
    | SsachC
    | SsachCG
    | SsachD
    | SsachDE
    | SsachDEV
    | SsachDI
    | SsachDIY
    | SsachDOM -- temp/hid
--  | SsachDR -- temp/hid
    | SsachE
    | SsachEM
    | SsachEW
    | SsachF
    | SsachFA
    | SsachFAG
    | SsachFET
    | SsachFD
    | SsachFG
    | SsachFIZ
    | SsachFL
    | SsachFTB -- temp/hid
    | SsachFUR
    | SsachG
    | SsachGA
    | SsachGB
    | SsachGD
    | SsachGIF -- temp/hid
    | SsachH
    | SsachHC
    | SsachHH -- temp/hid
    | SsachHI
    | SsachHO
    | SsachHW
    | SsachI
    | SsachINT
    | SsachIZD -- temp/hid
    | SsachJA
--  | SsachLS
    | SsachMA
    | SsachMC
    | SsachMDK
    | SsachME
    | SsachMG
    | SsachMLP
    | SsachMMO
    | SsachMO
    | SsachMOV -- temp/hid
    | SsachMU
    | SsachMUS
    | SsachNE
    | SsachO
    | SsachP
    | SsachPA
    | SsachPO
    | SsachPR
    | SsachPSY
    | SsachPVC -- temp/hid
    | SsachR
    | SsachRA
    | SsachRE
    | SsachRF
    | SsachRM
    | SsachS
    | SsachSCI
    | SsachSEX
    | SsachSF
    | SsachSN
    | SsachSOC
    | SsachSP
    | SsachSPC
    | SsachT
    | SsachTD
    | SsachTES
    | SsachTO
    | SsachTRV -- temp/hid
    | SsachTV
    | SsachUN
    | SsachVG
    | SsachVN
    | SsachW
    | SsachWEB -- temp/hid
    | SsachWH
    | SsachWM
    | SsachWP
    | SsachWR
  deriving (Eq, Read, Show, Enum, Bounded, Ord)

instance NFData Board

allSsachBoards :: [Board]
allSsachBoards = [minBound..maxBound]

-- | See "BlastItWithPiss.sortSsachBoardsByPopularity".
-- TODO Last update: Mon, 29 Oct 2012 22:49:11 +0200
-- TODO update silently in app background
ssachBoardsSortedByPostRate :: [(Board, Int)]
ssachBoardsSortedByPostRate =
    [(SsachB,2774)
    ,(SsachMMO,594)
    ,(SsachVG,486)
    ,(SsachPO,215)
    ,(SsachAU,169)
    ,(SsachA,163)
    ,(SsachMLP,132)
    ,(SsachSOC,131)
    ,(SsachSEX,122)
    ,(SsachT,101)
    ,(SsachFAG,66)
    ,(SsachWM,61)
    ,(SsachHW,53)
    ,(SsachRF,48)
    ,(SsachFTB,48)
    ,(SsachGA,40)
    ,(SsachFIZ,40)
    ,(SsachMA,38)
    ,(SsachCG,32)
    ,(SsachMU,31)
    ,(SsachDEV,31)
    ,(SsachTV,29)
    ,(SsachMOV,28)
    ,(SsachS,25)
    ,(SsachTES,23)
    ,(SsachFA,22)
    ,(SsachPA,19)
    ,(SsachBO,18)
    ,(SsachSPC,17)
    ,(SsachMUS,16)
    ,(SsachFET,16)
    ,(SsachSP,15)
    ,(SsachPR,14)
    ,(SsachMC,14)
    ,(SsachHI,14)
    ,(SsachHC,14)
    ,(SsachE,13)
    ,(SsachPSY,11)
    ,(SsachGD,11)
    ,(SsachFD,11)
    ,(SsachME,10)
    ,(SsachG,10)
    ,(SsachBIZ,10)
    ,(SsachW,8)
    ,(SsachWR,7)
    ,(SsachRE,7)
    ,(SsachP,7)
    ,(SsachI,6)
    ,(SsachEW,6)
    ,(SsachBI,6)
    ,(SsachUN,5)
    ,(SsachFG,5)
    ,(SsachD,5)
    ,(SsachWP,4)
    ,(SsachVN,4)
    ,(SsachTO,4)
    ,(SsachSN,4)
    ,(SsachRA,4)
    ,(SsachMO,4)
    ,(SsachMG,4)
    ,(SsachIZD,4)
    ,(SsachGIF,4)
    ,(SsachFUR,4)
    ,(SsachFL,4)
    ,(SsachDIY,4)
    ,(SsachSF,3)
    ,(SsachSCI,3)
    ,(SsachNE,3)
    ,(SsachEM,3)
    ,(SsachDOM,3)
    ,(SsachDI,3)
    ,(SsachAPP,3)
    ,(SsachAA,3)
    ,(SsachWH,2)
    ,(SsachWEB,2)
    ,(SsachR,2)
    ,(SsachPVC,2)
    ,(SsachJA,2)
    ,(SsachHH,2)
    ,(SsachDE,2)
    ,(SsachC,2)
    ,(SsachBG,2)
    ,(SsachTRV,1)
    ,(SsachTD,1)
    ,(SsachRM,1)
    ,(SsachO,1)
    ,(SsachMDK,1)
    ,(SsachINT,1)
    ,(SsachHO,1)
    ,(SsachH,1)
    ,(SsachGB,1)
    ,(SsachF,1)
    ,(SsachASYLUM,1)
    ,(SsachABU,1)]

readBoard :: String -> Maybe Board
readBoard ('/':x@(_:_))
    | last x == '/' = readMay $ ("Ssach" ++) $ map toUpper $ init x
    | otherwise = Nothing
readBoard _ = Nothing

renderBoard :: (Monoid a, IsString a) => Board -> a
renderBoard b = "/" <> fromString (map toLower $ fromJust $ stripPrefix "Ssach" $ show b) <> "/"

ssach :: IsString a => a
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
ssachBumpLimit SsachB = 513
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
    [partBS "anon_icon" "-1" | board == SsachPO]

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
