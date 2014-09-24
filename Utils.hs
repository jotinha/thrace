module Utils (module Utils) where

import GHC.Exts (sortWith)

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f xs = head $ sortWith f xs

maximumWith :: Ord b => (a -> b) -> [a] -> a
maximumWith f xs = last $ sortWith f xs

limits  :: Ord t => t -> t -> t -> t
limits minv maxv = (max minv) . (min maxv)
