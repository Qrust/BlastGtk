{-# OPTIONS_GHC -fno-warn-orphans #-}
module BlastItWithPiss.Proxy
    (Proxy(..)
    ,readProxy
    ,fromProxy
    ) where
import Import

import Network.Socket.Internal
import Network.Socks5
import qualified Network.HTTP.Conduit as HTTP

import qualified Text.Show as Show
import qualified Data.ByteString.Char8 as B8

data Proxy
    = HttpProxy !HTTP.Proxy
    | SocksProxy !SocksConf
    | NoProxy
  deriving (Eq, Ord)

readProxy :: Bool -> String -> Maybe Proxy
readProxy isSocks s =
    case break (==':') (dropWhile isSpace s) of
        (host@(_:_), (_:port)) ->
            if isSocks
                then SocksProxy . defaultSocksConf host . PortNum <$> readMay port
                else HttpProxy . HTTP.Proxy (fromString host) <$> readMay port
        _ -> Nothing

fromProxy :: a -> (Proxy -> a) -> Proxy -> a
fromProxy v _ NoProxy = v
fromProxy _ f p = f p

instance Show Proxy where
    show (HttpProxy (HTTP.Proxy h p)) =
        "{" ++ B8.unpack h ++ ":" ++ show p ++ "}"
    show (SocksProxy (SocksConf h (PortNum p) _)) =
        "{" ++ h ++ ":" ++ show p ++ "}"
    show NoProxy = "{@}"

instance Eq SocksConf where
    (SocksConf h1 p1 v1) == (SocksConf h2 p2 v2) =
        h1==h2 && p1 == p2 && v1 == v2

instance Ord HTTP.Proxy where
    compare (HTTP.Proxy h1 p1) (HTTP.Proxy h2 p2) =
        compare h1 h2 <> compare p1 p2

instance Ord SocksConf where
    compare (SocksConf h1 p1 v1) (SocksConf h2 p2 v2) =
        compare h1 h2 <> compare p1 p2 <> compare v1 v2

instance NFData HTTP.Proxy where
    rnf (HTTP.Proxy h p) = rnf (h,p)

instance NFData PortNumber where
    rnf (PortNum p) = rnf p

instance NFData SocksConf where
    rnf (SocksConf h p v) = rnf (h,p,v)

instance NFData Proxy where
    rnf (HttpProxy p) = rnf p
    rnf (SocksProxy p) = rnf p
    rnf NoProxy = ()
