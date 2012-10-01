module Import
    (module A
    ,LByteString
    ,LText
    ,show
    ,(>$>)
    ,if'
    ,ifM
    ,whenM
    ,unlessM
    ,fromLeft
    ,fromRight
    ,takeUntil
    ,dropUntil
    ,dropUntilLP
    ,findMap
    ,findMapM
    ,stripPrefixOfP
    ,isPrefixOfP
    ,isInfixOfP
    ,getPrefixOfP
    ,getInfixOfP
    ,delimitBy
    ,delimitByLE
    ,slice
    ,findWithSurroundings
    ,findWithSurroundingsLE
    ,appfst
    ,appsnd
    ,justIf
    ,untilJust
    ,untilNothing
    ,fromTrySome
    ,modifyIORefM
    ) where
import Prelude as A hiding (show, appendFile, getContents, getLine, interact, readFile, writeFile, catch, ioError)
import System.IO as A hiding (readFile, writeFile)
import System.IO.UTF8 as A (readFile, writeFile)
import Data.Monoid as A
import Data.Maybe as A
import Data.List as A
import Data.Char as A
import Control.Applicative as A hiding (empty)
import Control.Monad as A
import Data.String as A
import Data.Either as A
import Debug.Trace as A
import Data.Typeable as A
import Data.Data as A
import Control.Arrow as A
import Data.Function as A
import Data.Default as A
import Safe as A
import Data.Text as A (Text)
import Data.Time as A
import Data.Time.Clock.POSIX as A
import Control.Monad.IO.Class as A
import Control.DeepSeq as A
import Data.ByteString as A (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import Data.ByteString.Char8 as A ()
import Data.ByteString.Lazy.Char8 as A ()
import Control.Exception.Lifted as A
import Control.Monad.Trans.Control as A
import qualified Data.Text.Lazy as LT (Text)
import qualified Text.Show as S
import Data.IORef as A

type LByteString = LB.ByteString

type LText = LT.Text

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString

instance NFData LB.ByteString where
    rnf r = LB.toChunks r `deepseq` ()
#endif

{-# INLINE show #-}
show :: (Show a, IsString b) => a -> b
show = fromString . S.show

{-# INLINE (>$>) #-}
infixl 0 >$>
(>$>) :: a -> (a -> b) -> b
a >$> b = b a

{-# INLINE if' #-}
if' :: Bool -> a -> a -> a
if' a b c = if a then b else c

{-# INLINE ifM #-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM a b c = a >>= \d -> if' d b c

{-# INLINE whenM #-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM i t = ifM i t (return ())

{-# INLINE unlessM #-}
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM i t = ifM i (return ()) t

{-# INLINE fromLeft #-}
fromLeft :: Either a b -> a
fromLeft = either id (error "fromLeft failed")

{-# INLINE fromRight #-}
fromRight :: Either a b -> b
fromRight = either (error "fromRight failed") id

{-# INLINE takeUntil #-}
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = takeWhile (not . p)

{-# INLINE dropUntil #-}
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p = dropWhile (not . p)

{-# INLINE dropUntilLP #-}
dropUntilLP :: [a -> Bool] -> [a] -> [a]
dropUntilLP ps xs = fromMaybe [] $ find (isPrefixOfP ps) $ tails xs

{-# INLINE findMap #-}
findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f xs = headMay (mapMaybe f xs)

findMapM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findMapM _ [] = return Nothing
findMapM f (x:xs) = do a <- f x
                       if isJust a
                        then return a
                        else findMapM f xs

stripPrefixOfP :: [a -> Bool] -> [a] -> Maybe [a]
stripPrefixOfP [] a = Just a
stripPrefixOfP _ [] = Nothing
stripPrefixOfP (p:ps) (x:xs) = if p x then stripPrefixOfP ps xs else Nothing

isPrefixOfP :: [a -> Bool] -> [a] -> Bool
isPrefixOfP [] _ = True
isPrefixOfP _ [] = False
isPrefixOfP (p:ps) (x:xs) = p x && isPrefixOfP ps xs

isInfixOfP :: [a -> Bool] -> [a] -> Bool
isInfixOfP predicates list = any (isPrefixOfP predicates) $ tails list

getPrefixOfP :: [a -> Bool] -> [a] -> Maybe [a]
getPrefixOfP = getPrefixOfP' []
  where getPrefixOfP' acc [] _ = Just acc
        getPrefixOfP' acc (p:ps) (x:xs) | p x = getPrefixOfP' (acc ++ [x]) ps xs
        getPrefixOfP' _ _ _ = Nothing

getInfixOfP :: [a -> Bool] -> [a] -> Maybe [a]
getInfixOfP ps xs = findMap (getPrefixOfP ps) $ tails xs

delimitBy :: (a -> Bool) -> [a] -> [[a]]
delimitBy _ [] = []
delimitBy f l =
    case break f l of
        (a, []) -> [a]
        (a, _:xs) -> a : delimitBy f xs

delimitByLE :: Eq a => [a] -> [a] -> [[a]]
delimitByLE _ [] = []
delimitByLE d l =
    case findWithSurroundingsLE d l of
        Just (a, _, b) -> a : delimitByLE d b
        Nothing -> [l]

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice len list =
    case splitAt len list of
        (s, ss) -> s : slice len ss

findWithSurroundings :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
findWithSurroundings p l =
    case break p l of
        (_, []) -> Nothing
        (f, (v:s)) -> Just (f, v, s)

findWithSurroundingsLE :: Eq a => [a] -> [a] -> Maybe ([a], [a], [a])
findWithSurroundingsLE = find' []
  where find' _ _ [] = Nothing
        find' pas pr l@(a:as)
            | Just ts <- stripPrefix pr l =
                Just (reverse pas, pr, ts)
            | otherwise = find' (a:pas) pr as

{-# INLINE appfst #-}
appfst :: (a -> c) -> (a, b) -> (c, b)
appfst a = a *** id

{-# INLINE appsnd #-}
appsnd :: (b -> d) -> (a, b) -> (a, d)
appsnd a = id *** a

{-# INLINE justIf #-}
justIf :: (a -> Bool) -> a -> Maybe a
justIf p a = if p a then Just a else Nothing

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = do x <- m
                 case x of
                    Just a -> return a
                    Nothing -> untilJust m

untilNothing :: (Monad m, Functor m) => m (Maybe a) -> m [a]
untilNothing m = do x <- m
                    case x of
                        Just a -> (a :) <$> untilNothing m
                        Nothing -> return []

fromTrySome :: MonadBaseControl IO m => m a -> m a -> m a
fromTrySome e m = do
    a <- try m
    case a of
        Left (_::SomeException) -> e
        Right z -> return z

modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM r m =
    writeIORef r =<< m =<< readIORef r
