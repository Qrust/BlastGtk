module Main where
import Prelude
import Control.Applicative
import Data.Maybe
import Updater.Manifest
import qualified Paths_blast_it_with_piss as Paths
import System.Console.Readline
import Text.ParserCombinators.ReadP
import Data.Version
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Crypto.Classes hiding (encode)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import System.Environment
import Control.Exception

emptyUpdate :: UpdateManifest
emptyUpdate = UpdateManifest
    {version = Paths.version
    ,binaryAndResourcesZipArchives = []
    ,imagePackZipArchives = []
    ,changelog = []
    }

main :: IO ()
main = do
    [rv,la,wa] <- getArgs
    let v = fst $ last $ readP_to_S parseVersion $ rv
    manifest' <- either (\(_::SomeException) -> emptyUpdate)
                        (fromMaybe emptyUpdate . decode . toLBS) <$> try (B.readFile "UPDATE_MANIFEST")
    chlog <- fromMaybe (error "Please write changelog") <$> readline "Changes in this version:\n"
    lasum <- renderMD5 . hash' <$> B.readFile la
    wasum <- renderMD5 . hash' <$> B.readFile wa
    let manifest = manifest'{version = v
                            ,binaryAndResourcesZipArchives =
                                 [(Linux, (la, lasum))
                                 ,(Windows, (wa, wasum))]
                            ,changelog = (v, chlog) : changelog manifest
                            }
    L.writeFile "UPDATE_MANIFEST" $ encodePretty manifest
    putStrLn "Success."
