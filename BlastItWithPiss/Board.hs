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

readBoard :: String -> Board
readBoard x | Just a <- readMay (map toUpper x) = a
            | otherwise = error $ "readBoard: No such board \"" ++ x ++ "\""

renderBoardWithoutSlashes :: IsString a => Board -> a
renderBoardWithoutSlashes = fromString . map toLower . show

renderSlashes :: (Monoid a, IsString a) => Board -> a
renderSlashes b = "/" <> renderBoardWithoutSlashes b <> "/"

ssach :: IsString a => a
ssach = "http://2ch.so"

ssachBoard :: (Monoid a, IsString a) => Board -> a
ssachBoard b = ssach <> renderSlashes b

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
hoptoparashaBoard b = hoptoparasha <> renderSlashes b

hoptoparashaThread :: (Monoid a, IsString a) => Board -> Int -> a
hoptoparashaThread b t = hoptoparashaBoard b <> "res/" <> show t <> ".html"

hoptoparashaPage :: (Monoid a, IsString a) => Board -> Int -> a
hoptoparashaPage b 0 = hoptoparashaBoard b
hoptoparashaPage b i = hoptoparashaBoard b <> show i <> ".html"

-- TODO add 2chnu
