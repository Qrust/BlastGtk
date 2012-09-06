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
    ,splitBy
    ,slice
    ,findWithSurroundings
    ,findWithSurroundingsLE
    ,tupapp
    ,justIf
    ,untilJust
    ) where
import Prelude as A hiding (show)
-- import Filesystem.Path as A
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
import Safe as A
import Data.ByteString as A (ByteString)
import Data.Text as A (Text)
import Data.Time as A
import Control.Monad.IO.Class as A
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Data.Text.Lazy as LT (Text)
import qualified Text.Show as S

type LByteString = LB.ByteString

type LText = LT.Text

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

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f l =
    case break f l of
        (a, []) -> [a]
        (a, _:xs) -> a : splitBy f xs

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice len list = case splitAt len list of (s, ss) -> s : slice len ss

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

{-# INLINE tupapp #-}
tupapp :: (a -> b) -> (a -> c) -> a -> (b, c)
tupapp f1 f2 = \a -> (f1 a, f2 a)

{-# INLINE justIf #-}
justIf :: (a -> Bool) -> a -> Maybe a
justIf p a = if p a then Just a else Nothing

{-# INLINE untilJust #-}
untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = do x <- m
                 case x of
                    Just a -> return a
                    Nothing -> untilJust m
