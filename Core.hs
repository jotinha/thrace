module Core (module Core) where
import GHC.Exts (sortWith)

-- Vector functions 

data Vector = Vector Float Float Float deriving (Show,Eq)

vvMap :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
vvMap f (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (f x1 x2) (f y1 y2) (f z1 z2)

vsMap :: (Float -> Float -> Float) -> Vector -> Float -> Vector
vsMap f (Vector x1 y1 z1) s = Vector (f x1 s) (f y1 s) (f z1 s)

--(.+.) = vvMap (+)
--(.+) =  vsMap (+)
--(+.) =  (.+)
--(.-.) = vvMap (-)
--(.-) =  vsMap (-)
--(-.) s v = (Vector s s s) .-. v
--(./) = vsMap (/)
--(.*) = vsMap (*)
--(*.) = (.*)
--vmagnitude v = sqrt $ dot v v
--vnormalize v = v ./ (vmagnitude v)
--dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- other

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f xs = head $ sortWith f xs

maximumWith :: Ord b => (a -> b) -> [a] -> a
maximumWith f xs = last $ sortWith f xs

limits  :: Ord t => t -> t -> t -> t
limits minv maxv = (max minv) . (min maxv)

class VectorOps a where
  
  (.+.) :: a -> a -> a
  (.-.) :: a -> a -> a
  (.+)  :: a -> Float -> a
  (.*)  :: a -> Float -> a
  vnegate :: a -> a
  vdot :: a -> a -> Float

  (+.)  :: Float -> a -> a
  (+.) = flip (.+)

  (.-)  :: a -> Float -> a
  (.-) v s = (.+) v (-s)
  
  (-.)  :: Float -> a -> a
  (-.) s v = s +. (vnegate v)
  
  (*.)  :: Float -> a -> a
  (*.) = flip (.*)

  (./)  :: a -> Float -> a
  (./) v s = (.*) v (1/s)

  vmagnitude :: a -> Float
  vmagnitude v = sqrt $ vdot v v

  vnormalize :: a -> a
  vnormalize v = v ./ (vmagnitude v)


instance VectorOps Vector where
  (Vector x1 y1 z1) .+. (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
  (Vector x1 y1 z1) .-. (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)
  (Vector x y z) .+ s = Vector (x + s)  (y + s)  (z +s)
  (Vector x y z) .* s = Vector (x*s) (y*s) (z*s)
  vdot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
  vnegate (Vector x y z) = Vector (-x) (-y) (-z)
--  s *. v = v .* s
  --v ./ s = v .* (1/s)

