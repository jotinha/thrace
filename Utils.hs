module Utils (module Utils) where

import GHC.Exts (sortWith)
import qualified Debug.Trace

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f xs = head $ sortWith f xs

maximumWith :: Ord b => (a -> b) -> [a] -> a
maximumWith f xs = last $ sortWith f xs

limits  :: Ord t => t -> t -> t -> t
limits minv maxv  v | v < minv = minv
                    | v > maxv = maxv
                    | otherwise = v

withinRange :: Ord t => t -> t -> t -> Bool
withinRange minx maxx x = x >= minx && x <= maxx

isEmpty :: [a] -> Bool
isEmpty xs = length xs == 0

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

isSomething :: Maybe a -> Bool
isSomething = not . isNothing

getSomething :: Maybe a -> a
getSomething (Just x) = x

sortTuple2 :: (Ord a) => (a,a) -> (a,a)
sortTuple2 (a,b) | a <= b    = (a,b)
                 | otherwise = (b,a)

debug = flip Debug.Trace.trace

-- clamped cosine
ccos = (max 0) . cos

-- finds root of equatio ax^2 + bx + cx = 0
rootspoly2 :: (Floating a, Fractional a, Ord a) => a -> a -> a -> Maybe (a,a)
rootspoly2 a b c | arg < 0    = Nothing
                 | otherwise  = Just $ (-b/2/a) `pm` ((sqrt arg)/2/a)
  where 
    arg = b*b - 4*a*c
    pm x y = (x + y, x - y)

xor :: Bool -> Bool -> Bool
xor = (/=)