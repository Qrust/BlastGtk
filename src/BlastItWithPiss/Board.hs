{-# OPTIONS_GHC -fno-cse #-}
module BlastItWithPiss.Board
    (Board(..)
    ,readBoard
    ,renderBoard

    -- * Ssach
    ,domainVar
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
    ) where
import Import
import Network.HTTP.Conduit.MultipartFormData

import System.IO.Unsafe
import Data.IORef

-- | Ssach boards.
data Board
    = SsachA
    | SsachAA
    | SsachABU
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
    | SsachMOBI
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
-- Last update: Mon, 30 Sep 2013 19:47:52 +0300
ssachBoardsSortedByPostRate :: [(Board, Int)]
ssachBoardsSortedByPostRate =
    [(SsachB     , 3351)
    ,(SsachVG    , 421 )
    ,(SsachMMO   , 285 )
    ,(SsachPO    , 242 )
    ,(SsachAU    , 227 )
    ,(SsachMLP   , 196 )
    ,(SsachMOBA  , 191 )
    ,(SsachA     , 124 )
    ,(SsachFTB   , 112 )
    ,(SsachTV    , 104 )
    ,(SsachSEX   , 95  )
    ,(SsachSOC   , 67  )
    ,(SsachG     , 65  )
    ,(SsachS     , 63  )
    ,(SsachWM    , 52  )
    ,(SsachWH    , 34  )
    ,(SsachHW    , 33  )
    ,(SsachASYLUM, 31  )
    ,(SsachMOBI  , 31  )
    ,(SsachBO    , 29  )
    ,(SsachFD    , 29  )
    ,(SsachCG    , 28  )
    ,(SsachDEV   , 28  )
    ,(SsachTES   , 27  )
    ,(SsachBIZ   , 26  )
    ,(SsachD     , 26  )
    ,(SsachGA    , 25  )
    ,(SsachMOV   , 24  )
    ,(SsachAA    , 22  )
    ,(SsachFA    , 19  )
    ,(SsachFAG   , 19  )
    ,(SsachGD    , 19  )
    ,(SsachFIZ   , 18  )
    ,(SsachPSY   , 17  )
    ,(SsachRF    , 16  )
    ,(SsachPR    , 15  )
    ,(SsachUN    , 15  )
    ,(SsachP     , 14  )
    ,(SsachME    , 13  )
    ,(SsachMU    , 13  )
    ,(SsachPVC   , 12  )
    ,(SsachBG    , 11  )
    ,(SsachT     , 11  )
    ,(SsachDIY   , 10  )
    ,(SsachE     , 10  )
    ,(SsachEW    , 10  )
    ,(SsachHI    , 10  )
    ,(SsachMA    , 10  )
    ,(SsachMUS   , 10  )
    ,(SsachC     , 9   )
    ,(SsachDE    , 9   )
    ,(SsachMG    , 8   )
    ,(SsachMO    , 8   )
    ,(SsachPA    , 8   )
    ,(SsachSN    , 8   )
    ,(SsachW     , 8   )
    ,(SsachFUR   , 7   )
    ,(SsachVN    , 7   )
    ,(SsachFG    , 6   )
    ,(SsachRA    , 6   )
    ,(SsachRE    , 6   )
    ,(SsachFET   , 5   )
    ,(SsachRM    , 5   )
    ,(SsachSPC   , 5   )
    ,(SsachBI    , 4   )
    ,(SsachFL    , 4   )
    ,(SsachWR    , 4   )
    ,(SsachDI    , 3   )
    ,(SsachEM    , 3   )
    ,(SsachFS    , 3   )
    ,(SsachHC    , 3   )
    ,(SsachMC    , 3   )
    ,(SsachNE    , 3   )
    ,(SsachSCI   , 3   )
    ,(SsachSF    , 3   )
    ,(SsachWP    , 3   )
    ,(SsachH     , 2   )
    ,(SsachHH    , 2   )
    ,(SsachHO    , 2   )
    ,(SsachI     , 2   )
    ,(SsachINT   , 2   )
    ,(SsachJA    , 2   )
    ,(SsachR     , 2   )
    ,(SsachWEB   , 2   )
    ,(SsachABU   , 1   )
    ,(SsachDOM   , 1   )
    ,(SsachF     , 1   )
    ,(SsachGB    , 1   )
    ,(SsachGIF   , 1   )
    ,(SsachIZD   , 1   )
    ,(SsachO     , 1   )
    ,(SsachSP    , 1   )
    ,(SsachTD    , 1   )
    ,(SsachTO    , 1   )
    ,(SsachTRV   , 1   )]

readBoard :: String -> Maybe Board
readBoard ('/':x@(_:_))
    | last x == '/' = readMay $ ("Ssach" ++) $ map toUpper $ init x
    | otherwise = Nothing
readBoard _ = Nothing

{-# INLINE renderBoard #-}
renderBoard :: (Monoid a, IsString a) => Board -> a
renderBoard b =
    "/" <>
        fromString (map toLower $ fromJust $ stripPrefix "Ssach" $ show b)
        <> "/"

{-# NOINLINE domainVar #-}
domainVar :: IORef String
domainVar = unsafePerformIO (newIORef "2ch.hk")

{-# NOINLINE ssach #-}
ssach :: (Monoid a, IsString a) => a
ssach = "http://" <> fromString (unsafePerformIO (readIORef domainVar))

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
ssachLengthLimit = 7168 -- max cyrillic characters, stupid ssach counts bytes
                        -- instead of unicode code points.

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
