module Main where
import Prelude()
import Import
import Data.Version
import Text.ParserCombinators.ReadP
import System.Environment

-- HORRIBLE HACK but i don't care.

main = do
    g <- getArgs
    cf <- readFile "blast-it-with-piss.cabal"
    let (a, x, b) = fromMaybe (error "Couldn't find version in cabal file, oh well.") $
            findWithSurroundings (isPrefixOf "version: ") (lines cf)
        oldv@Version{versionBranch=[v1,v2,v3,v4]} =
            fst $ last $ (readP_to_S parseVersion) (fromJust $ stripPrefix "version: " x)
    if "--get" `elem` g
        then putStr $ showVersion oldv
        else do let newv = oldv{versionBranch=[v1, v2, v3, v4+1]}
                putStrLn $ "Old version: " ++ showVersion oldv
                putStrLn $ "New version: " ++ showVersion newv
                writeFile "blast-it-with-piss.cabal.bak" cf
                writeFile "blast-it-with-piss.cabal" $
                    unlines (a ++ ["version: " ++ showVersion newv] ++ b)
    
