module BlastItWithPiss.Board where
import Import

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
    deriving (Eq, Show, Enum, Bounded, Ord)

readBoard :: String -> Board
readBoard "a" = A
readBoard "aa" = AA
readBoard "abu" = ABU
readBoard "app" = APP
readBoard "asylum" = ASYLUM
readBoard "au" = AU
readBoard "b" = B
readBoard "bb" = BB
readBoard "bg" = BG
readBoard "bi" = BI
readBoard "biz" = BIZ
readBoard "bo" = BO
readBoard "c" = C
readBoard "cg" = CG
readBoard "d" = D
readBoard "de" = DE
readBoard "dev" = DEV
readBoard "di" = DI
readBoard "diy" = DIY
readBoard "em" = EM
readBoard "ew" = EW
readBoard "f" = F
readBoard "fa" = FA
readBoard "fag" = FAG
readBoard "fd" = FD
readBoard "fg" = FG
readBoard "fiz" = FIZ
readBoard "fl" = FL
readBoard "fur" = FUR
readBoard "g" = G
readBoard "ga" = GA
readBoard "gb" = GB
readBoard "gd" = GD
readBoard "h" = H
readBoard "hi" = HI
readBoard "ho" = HO
readBoard "hw" = HW
readBoard "i" = I
readBoard "int" = INT
readBoard "ja" = JA
readBoard "ls" = LS
readBoard "ma" = MA
readBoard "mc" = MC
readBoard "mdk" = MDK
readBoard "me" = ME
readBoard "mg" = MG
readBoard "mlp" = MLP
readBoard "mmo" = MMO
readBoard "mo" = MO
readBoard "mu" = MU
readBoard "mus" = MUS
readBoard "ne" = NE
readBoard "o" = O
readBoard "p" = P
readBoard "pa" = PA
readBoard "per" = PER
readBoard "po" = PO
readBoard "pr" = PR
readBoard "psy" = PSY
readBoard "r" = R
readBoard "ra" = RA
readBoard "re" = RE
readBoard "rf" = RF
readBoard "rm" = RM
readBoard "s" = S
readBoard "sci" = SCI
readBoard "sex" = SEX
readBoard "sf" = SF
readBoard "sn" = SN
readBoard "soc" = SOC
readBoard "sp" = SP
readBoard "spc" = SPC
readBoard "t" = T
readBoard "td" = TD
readBoard "tes" = TES
readBoard "to" = TO
readBoard "tv" = TV
readBoard "un" = UN
readBoard "vg" = VG
readBoard "vn" = VN
readBoard "w" = W
readBoard "wh" = WH
readBoard "wm" = WM
readBoard "wp" = WP
readBoard "wr" = WR
readBoard x = error $ "readBoard: No such board \"" ++ x ++ "\""

render :: IsString a => Board -> a
render A = "a"
render AA = "aa"
render ABU = "abu"
render APP = "app"
render ASYLUM = "asylum"
render AU = "au"
render B = "b"
render BB = "bb"
render BG = "bg"
render BI = "bi"
render BIZ = "biz"
render BO = "bo"
render C = "c"
render CG = "cg"
render D = "d"
render DE = "de"
render DEV = "dev"
render DI = "di"
render DIY = "diy"
render EM = "em"
render EW = "ew"
render F = "f"
render FA = "fa"
render FAG = "fag"
render FD = "fd"
render FG = "fg"
render FIZ = "fiz"
render FL = "fl"
render FUR = "fur"
render G = "g"
render GA = "ga"
render GB = "gb"
render GD = "gd"
render H = "h"
render HI = "hi"
render HO = "ho"
render HW = "hw"
render I = "i"
render INT = "int"
render JA = "ja"
render LS = "ls"
render MA = "ma"
render MC = "mc"
render MDK = "mdk"
render ME = "me"
render MG = "mg"
render MLP = "mlp"
render MMO = "mmo"
render MO = "mo"
render MU = "mu"
render MUS = "mus"
render NE = "ne"
render O = "o"
render P = "p"
render PA = "pa"
render PER = "per"
render PO = "po"
render PR = "pr"
render PSY = "psy"
render R = "r"
render RA = "ra"
render RE = "re"
render RF = "rf"
render RM = "rm"
render S = "s"
render SCI = "sci"
render SEX = "sex"
render SF = "sf"
render SN = "sn"
render SOC = "soc"
render SP = "sp"
render SPC = "spc"
render T = "t"
render TD = "td"
render TES = "tes"
render TO = "to"
render TV = "tv"
render UN = "un"
render VG = "vg"
render VN = "vn"
render W = "w"
render WH = "wh"
render WM = "wm"
render WP = "wp"
render WR = "wr"

renderSlashes :: (Monoid a, IsString a) => Board -> a
renderSlashes b = "/" <> render b <> "/"

ssach :: IsString a => a
ssach = "http://2ch.so"

ssachBoard :: (Monoid a, IsString a) => Board -> a
ssachBoard b = ssach <> renderSlashes b

ssachThread :: (Monoid a, IsString a) => Board -> Int -> a
ssachThread b t = ssachBoard b <> "res/" <> show t <> ".html"

ssachPage :: (Monoid a, IsString a) => Board -> Int -> a
ssachPage b 0 = ssachBoard b
ssachPage b i = ssachBoard b <> show i <> ".html"

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
