module Main where
import Import
import Updater.Manifest
import Updater.UnpackZip
import System.Directory
import System.FilePath
import Data.Aeson
import System.Environment.Executable
import Network.HTTP.Conduit
import Graphics.UI.Gtk hiding (Entry)
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L
import qualified Paths_blast_it_with_piss as Paths

{-
data UpdaterConf = UpdaterConf
        {updateWithoutAsking :: Bool
        ,manifestUrl :: String
        }
-}

{-
- скачиваем манифест с файла, в манифесте версия, список архивов и чексумм
- скачиваем архивы, сверяем чексуммы, если что-то не совпадает выдаем диалог типа "Retry-Abort-Cancel"
- создаем папку blast.old.$СТАРАЯ-ВЕРСИЯ, если уже есть то добавляем цифры
    -- переименовываем файлы и директории для которых есть версия из архива в бэкап-папку.
    -- ^ как часть распаковки уже
- распаковываем.
-}

errorNoBuildAvailable :: Platform -> String
errorNoBuildAvailable Linux = "Случилось абсолютно невозможное, не обнаружено версии вайпалки для единственной операционной системы!"
errorNoBuildAvailable Windows = "Не обнаружено версии вайпалки для утятницы \"Пекач\"\nРешение:\n1. Соснуть хуйцов\n2.Сделать бочку."
errorNoBuildAvailable Mac = "Не обнаружено версии вайпалки для мака, возможно эта ошибка появляется потому что MAKOBLYADI SOSNOOLEY\nРешение:\n1.Пососать разложившийся хуец жопса\nАльтернативное решение:\n1. Связаться с автором(через тред, жаббер kudah@jabber.ru, скайп kudahkukarek или через гитхаб)\n2. Скомпилять версию для мака\n3. Пососать разложившийся хуец жопса."

needUpdate :: UpdateManifest -> Bool
needUpdate um = version um > Paths.version

downloadManifest :: IO (Either SomeException UpdateManifest)
downloadManifest = try $ do
    fromMaybe (error "Couldn't parse update manifest as valid json") . decode' <$>
        simpleHttp "https://raw.github.com/exbb2/BlastItWithPiss/master/UPDATE_MANIFEST"

main :: IO ()
main = undefined
