module Main where
import Prelude
import Data.Version
import System.Directory
import System.FilePath
import System.Environment.Executable
import Network.HTTP.Conduit
import Graphics.UI.Gtk

type URL = String

type MD5Sum = String

data Platform = Linux | Windows | Mac

{-
data UpdateManifest = UpdateManifest
    {root :: URL
    ,version :: Version
    ,files :: [(String, MD5Sum)]
    ,binaries :: [(String, MD5Sum, Platform)]
    ,popupMessage :: Maybe String
    ,configCompatible :: Version -- ^ если наша версия — копируем конфиг из .old.
    }

- скачиваем манифест с файла, в манифесте версия, список файлов и чексумм.
- создаем папку blast.old, если уже есть то инкрементируем цифры типа blast.old2, blast.old999
  пока не найдем незанятое имя.
- _перемещаем_(не переименовываем) файлы которые нужно заменить в папку
- скачиваем файлы, сверяем чексуммы, если что-то не совпадает отменяем всё нахуй и перемещаем файлы обратно из blast.old
- blast.old не удаляем даже при успешном апдейте(пока)
-}

errorNoBuildAvailable :: Platform -> String
errorNoBuildAvailable Linux = "Случилось абсолютно невозможное, не обнаружено версии вайпалки для единственной операционной системы!"
errorNoBuildAvailable Windows = "Не обнаружено версии вайпалки для утятницы \"Пекач\"\nРешение:\n1. Соснуть хуйцов\n2.Сделать бочку."
errorNoBuildAvailable Mac = "Не обнаружено версии вайпалки для мака, возможно эта ошибка появляется потому что MAKOBLYADI SOSNOOLEY\nРешение:\n1.Пососать разложившийся хуец жопса\nАльтернативное решение:\n1. Связаться с автором(через news-конфу колчана, по скайпу kudahkukarek, или через открытие баг-репорта в Issues репозитория вайпалки)\n2. Скомпилять версию для мака\n3. Пососать разложившийся хуец жопса."

keyValuesToManifest :: [(String, String)] -> Either String UpdateManifest
keyValuesToManifest = undefined

data UpdateManifest = UpdateManifest
    {version :: Version
    ,binaryAndResourcesZipArchives :: [(URL, MD5Sum)]
    ,imagePackZipArchives :: [(URL, MD5Sum)]
    ,configCompatible :: Version -- ^ Если наша версия или ниже, то оставляем конфиг, в противном случае бэкапим.
    }
{-
- скачиваем манифест с файла, в манифесте версия, список архивов и чексумм
- скачиваем архивы, сверяем чексуммы, если что-то не совпадает выдаем диалог типа "Retry-Abort-Cancel"
- создаем папку blast.old, если уже есть то инкрементируем цифры типа blast.old2, blast.old999
  пока не найдем незанятое имя.
- переименовываем файлы и директории для которых есть версия из архива в бэкап-папку.
- распаковываем.
-}

data Outcome = Success
             | ChecksumMismatch
             | ServersUnreachable
             | NoBuildAvailable Platform
             | UnparseableManifest
             | HttpExc HttpException

main :: IO ()
main = undefined
