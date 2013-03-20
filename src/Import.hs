module Import
    (module A
    ,module Import
    ) where
#ifdef TEST
import Debug.Trace as A
#endif
import Safe as A
import Prelude as A hiding (show, appendFile, getContents, getLine, interact
                           ,readFile, writeFile
#if !MIN_VERSION_base(4,6,0)
                           ,catch
#endif
                           ,ioError, log, (++))
import System.IO as A hiding (readFile, writeFile, appendFile)
import Data.Monoid as A
import Data.Maybe as A
import Data.List as A hiding ((++))
import Data.Char as A
import Control.Applicative as A hiding (empty)
import Control.Monad as A
import Data.String as A
import Data.Either as A
import Data.Typeable as A
import Control.Arrow as A
import Data.Function as A
import Data.Default as A
import Data.IORef as A
import Data.Time as A
import Control.Monad.IO.Class as A
import Control.DeepSeq as A
import Control.Monad.Trans.Control as A
import Control.Exception.Lifted as A
import Data.ByteString as A (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text as A (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as LT
import qualified Text.Show as S

import Data.ByteString.Char8 as A ()
import Data.ByteString.Lazy.Char8 as A ()

-- * Strings

type LByteString = LB.ByteString

type LText = LT.Text

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString

instance NFData LB.ByteString where
    rnf r = LB.toChunks r `deepseq` ()
#endif

{-# INLINE toLBS #-}
toLBS :: ByteString -> LByteString
#if MIN_VERSION_bytestring(0,10,0)
toLBS = LB.fromStrict
#else
toLBS x = LB.fromChunks [x]
#endif

{-# INLINE show #-}
show :: (Show a, IsString b) => a -> b
show = fromString . S.show

{-# INLINE decodeUtf8 #-}
decodeUtf8 :: ByteString -> Text
decodeUtf8 = TE.decodeUtf8With TE.lenientDecode

{-# INLINE (++) #-}
(++) :: Monoid a => a -> a -> a
(++) = mappend

-- * CONTROL

{-# INLINE (&) #-}
infixl 1 &
(&) :: a -> (a -> b) -> b
a & b = b $ a

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

{-# INLINE whenJust #-}
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mb m = maybe (return ()) m mb

{-# INLINE whenJustM #-}
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mmb m = maybe (return ()) m =<< mmb

{-# INLINE fromMaybeM #-}
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM _nothing m = do
    x <- m
    case x of
      Just _just -> return _just
      Nothing -> _nothing

{-# INLINE fromLeft #-}
fromLeft :: Either a b -> a
fromLeft = either id (error "fromLeft failed")

{-# INLINE fromRight #-}
fromRight :: Either a b -> b
fromRight = either (error "fromRight failed") id

{-# INLINE justIf #-}
justIf :: (a -> Bool) -> a -> Maybe a
justIf p a = if p a then Just a else Nothing

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = maybe (untilJust m) return =<< m

untilNothing :: (Monad m, Functor m) => m (Maybe a) -> m [a]
untilNothing m = do
    x <- m
    case x of
        Just a -> (a :) <$> untilNothing m
        Nothing -> return []

{-# INLINE io #-}
-- | 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

{-# INLINE fromIOException #-}
fromIOException :: MonadBaseControl IO m => m a -> m a -> m a
fromIOException handler = handle (\(_ :: IOException) -> handler)

-- * MISC

{-# INLINE modifyIORefM #-}
modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM r m = writeIORef r =<< m =<< readIORef r

-- * LISTS

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM m (x:xs) = ifM (m x) (return True) (anyM m xs)

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
findMapM f (x:xs) = do
    a <- f x
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

{-# INLINABLE isInfixOfP #-}
isInfixOfP :: [a -> Bool] -> [a] -> Bool
isInfixOfP predicates list = any (isPrefixOfP predicates) $ tails list

{-# INLINABLE getPrefixOfP #-}
getPrefixOfP :: [a -> Bool] -> [a] -> Maybe [a]
getPrefixOfP = getPrefixOfP' []
  where getPrefixOfP' acc [] _ = Just acc
        getPrefixOfP' acc (p:ps) (x:xs) | p x = getPrefixOfP' (acc ++ [x]) ps xs
        getPrefixOfP' _ _ _ = Nothing

{-# INLINABLE getInfixOfP #-}
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

{-# INLINABLE slice #-}
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
