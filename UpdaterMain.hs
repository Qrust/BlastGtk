module Main where
import Prelude
import System.Directory
import System.FilePath
import System.Environment.Executable
import Network.HTTP.Conduit

type URL = String

type MD5Sum = String

data UpdateManifest = UpdateManifest
    {root :: URL
    ,date :: ()
    ,files :: [(String, MD5Sum)]
    ,popupMessage :: Maybe String
    }

{-
- скачиваем манифест с файла, в манифесте таймштамп, список файлов и чексумм.
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
