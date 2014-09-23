module Core (module Core) where


import GHC.Exts (sortWith)


-- Vector functions 

data Vector = Vector Float Float Float deriving (Show,Eq,Array)
  
vdot :: Vector -> Vector -> Float
(Vector x1 y1 z1) `vdot` (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

vminus :: Vector -> Vector -> Vector
(Vector x1 y1 z1) `vminus` (Vector x2 y2 z2) = Vector (x1-x2) (y1-y2) (z1-z2)

vplus :: Vector -> Vector -> Vector
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x2 + x1) (y2 + y1) (z2 + z1)

splus :: Vector -> Float -> Vector
(Vector x1 y1 z1) `splus` s = Vector (s+x1) (s+y1) (s+z1)

smult :: Vector -> Float -> Vector
(Vector x1 y1 z1) `smult` s = Vector (s*x1) (s*y1) (s*z1)

sdiv :: Vector -> Float -> Vector
(Vector x1 y1 z1) `sdiv` s = Vector (x1/s) (y1/s) (z1/s)

vsq :: Vector -> Float
vsq v = v `vdot` v

vnormalize :: Vector -> Vector
vnormalize v =  v `sdiv` (sqrt (vsq v))


-- other

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f xs = head $ sortWith f xs

maximumWith :: Ord b => (a -> b) -> [a] -> a
maximumWith f xs = last $ sortWith f xs

limits  :: Ord t => t -> t -> t -> t
limits minv maxv = (max minv) . (min maxv)

class Array where
  (-) :: Vector a => a -> a -> a
  (+) :: Vector a => a -> a -> a