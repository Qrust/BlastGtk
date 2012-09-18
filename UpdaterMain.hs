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

-- FIXME Может легче просто скачать соответствующий тарболл?

data Platform = Windows | Linux

data UpdateManifest = UpdateManifest
    {root :: URL
    ,version :: Version
    ,files :: [(String, MD5Sum)]
    ,binaries :: [(String, MD5Sum, Platform)]
    ,popupMessage :: Maybe String
    ,configCompatible :: Version -- ^ если наша версия — копируем конфиг из .old.
    }

{-
- скачиваем манифест с файла, в манифесте версия, список файлов и чексумм.
- создаем папку blast.old, если есть то инкрементируем цифры типа blast.old2, blast.old999
  пока не найдем незанятое имя.
- _перемещаем_(не переименовываем) файлы которые нужно заменить в папку
- скачиваем файлы, сверяем чексуммы, если что-то не совпадает отменяем всё нахуй и перемещаем файлы обратно из blast.old
- blast.old не удаляем даже при успешном апдейте(пока)
-}

data Outcome = Success
             | ChecksumMismatch
             | ServersUnreachable
             | HttpExc HttpException

main :: IO ()
main = undefined
