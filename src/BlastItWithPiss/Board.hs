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
    | SsachFS
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
    | SsachME
    | SsachMG
    | SsachMLP
    | SsachMMO
    | SsachMO
    | SsachMOBA
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
-- TODO Last update: Tue, 02 Jul 2013 23:38:45 +0300
-- TODO update silently in app background
ssachBoardsSortedByPostRate :: [(Board, Int)]
ssachBoardsSortedByPostRate =
    [(SsachB,2576)
    ,(SsachVG,364)
    ,(SsachMLP,173)
    ,(SsachMOBA,150)
    ,(SsachSEX,110)
    ,(SsachMMO,100)
    ,(SsachA,94)
    ,(SsachSOC,87)
    ,(SsachWM,60)
    ,(SsachRF,52)
    ,(SsachUN,48)
    ,(SsachFAG,47)
    ,(SsachS,39)
    ,(SsachCG,36)
    ,(SsachAU,36)
    ,(SsachT,35)
    ,(SsachWH,31)
    ,(SsachTES,29)
    ,(SsachSPC,28)
    ,(SsachDEV,28)
    ,(SsachMOV,27)
    ,(SsachAPP,25)
    ,(SsachGA,23)
    ,(SsachFA,23)
    ,(SsachHW,22)
    ,(SsachMU,21)
    ,(SsachD,21)
    ,(SsachBI,19)
    ,(SsachFIZ,18)
    ,(SsachBIZ,17)
    ,(SsachPR,15)
    ,(SsachHO,15)
    ,(SsachVN,14)
    ,(SsachG,14)
    ,(SsachTV,13)
    ,(SsachSF,13)
    ,(SsachME,13)
    ,(SsachMC,13)
    ,(SsachMUS,12)
    ,(SsachP,11)
    ,(SsachPO,10)
    ,(SsachDIY,10)
    ,(SsachHI,9)
    ,(SsachHC,9)
    ,(SsachWR,8)
    ,(SsachMO,8)
    ,(SsachFL,8)
    ,(SsachSP,7)
    ,(SsachRM,7)
    ,(SsachRE,7)
    ,(SsachPSY,7)
    ,(SsachPA,7)
    ,(SsachFG,7)
    ,(SsachE,7)
    ,(SsachMA,6)
    ,(SsachFTB,6)
    ,(SsachEM,5)
    ,(SsachBO,5)
    ,(SsachHH,4)
    ,(SsachH,4)
    ,(SsachSN,3)
    ,(SsachSCI,3)
    ,(SsachPVC,3)
    ,(SsachI,3)
    ,(SsachFUR,3)
    ,(SsachFD,3)
    ,(SsachFET,3)
    ,(SsachW,2)
    ,(SsachTD,2)
    ,(SsachNE,2)
    ,(SsachMG,2)
    ,(SsachIZD,2)
    ,(SsachGB,2)
    ,(SsachFS,2)
    ,(SsachEW,2)
    ,(SsachDE,2)
    ,(SsachC,2)
    ,(SsachASYLUM,2)
    ,(SsachAA,2)
    ,(SsachWP,1)
    ,(SsachWEB,1)
    ,(SsachTRV,1)
    ,(SsachRA,1)
    ,(SsachR,1)
    ,(SsachGIF,1)
    ,(SsachGD,1)
    ,(SsachDOM,1)
    ,(SsachTO,0)
    ,(SsachO,0)
    ,(SsachJA,0)
    ,(SsachINT,0)
    ,(SsachF,0)
    ,(SsachDI,0)
    ,(SsachBG,0)
    ,(SsachABU,0)]

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
