module Vector (module Vector) where

-- Vector3 functions 

data Vector2 = Vector2 Float Float deriving (Show,Eq)
data Vector3 = Vector3 Float Float Float deriving (Show,Eq)
data Vector4 = Vector4 Float Float Float Float deriving (Show,Eq)


--vvMap :: (Float -> Float -> Float) -> Vector3 -> Vector3 -> Vector3
--vvMap f (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (f x1 x2) (f y1 y2) (f z1 z2)

--vsMap :: (Float -> Float -> Float) -> Vector3 -> Float -> Vector3
--vsMap f (Vector3 x1 y1 z1) s = Vector3 (f x1 s) (f y1 s) (f z1 s)

--(.+.) = vvMap (+)
--(.+) =  vsMap (+)
--(+.) =  (.+)
--(.-.) = vvMap (-)
--(.-) =  vsMap (-)
--(-.) s v = (Vector3 s s s) .-. v
--(./) = vsMap (/)
--(.*) = vsMap (*)
--(*.) = (.*)
--vmagnitude v = sqrt $ dot v v
--vnormalize v = v ./ (vmagnitude v)
--dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2


class Vector a where
  
  (.+.) :: a -> a -> a
  (.-.) :: a -> a -> a
  (.+)  :: a -> Float -> a
  (.*)  :: a -> Float -> a
  vnegate :: a -> a
  vdot :: a -> a -> Float
  vfill :: Float -> a

  -- implemented
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


instance Vector Vector2 where
  (Vector2 x1 y1) .+. (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
  (Vector2 x1 y1) .-. (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)
  (Vector2 x y) .+ s = Vector2 (x + s)  (y + s)
  (Vector2 x y) .* s = Vector2 (x*s) (y*s)
  vdot (Vector2 x1 y1) (Vector2 x2 y2) = x1*x2 + y1*y2
  vnegate (Vector2 x y) = Vector2 (-x) (-y)
  vfill s = Vector2 s s

instance Vector Vector3 where
  (Vector3 x1 y1 z1) .+. (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vector3 x1 y1 z1) .-. (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vector3 x y z) .+ s = Vector3 (x + s)  (y + s)  (z +s)
  (Vector3 x y z) .* s = Vector3 (x*s) (y*s) (z*s)
  vdot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
  vnegate (Vector3 x y z) = Vector3 (-x) (-y) (-z)
  vfill s = Vector3 s s s

instance Vector Vector4 where
  (Vector4 x1 y1 z1 w1) .+. (Vector4 x2 y2 z2 w2) = Vector4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (Vector4 x1 y1 z1 w1) .-. (Vector4 x2 y2 z2 w2) = Vector4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (Vector4 x y z w) .+ s = Vector4 (x + s)  (y + s)  (z +s) (w +s)
  (Vector4 x y z w) .* s = Vector4 (x*s) (y*s) (z*s) (w*s)
  vdot (Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2
  vnegate (Vector4 x y z w) = Vector4 (-x) (-y) (-z) (-w)
  vfill s = Vector4 s s s s
