module Utils (module Utils) where

import GHC.Exts (sortWith)
import qualified Debug.Trace

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f xs = head $ sortWith f xs

maximumWith :: Ord b => (a -> b) -> [a] -> a
maximumWith f xs = last $ sortWith f xs

limits  :: Ord t => t -> t -> t -> t
limits minv maxv = (max minv) . (min maxv)

withinRange :: Ord t => t -> t -> t -> Bool
withinRange minx maxx x = x >= minx && x <= maxx

isEmpty :: [a] -> Bool
isEmpty xs = length xs == 0

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

isSomething :: Maybe a -> Bool
isSomething = not . isNothing

debug = flip Debug.Trace.trace

-- clamped cosine
ccos = (max 0) . cos