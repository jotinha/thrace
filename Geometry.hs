module Geometry (module Geometry) where

import Vector

data Geometry = 
  Sphere Vector3 Float |  -- center and radius
  Plane Vector3 Float  |  -- normal and constant
  Triangle Vector3 Vector3 Vector3 |
  AABox Vector3 Vector3  --axis aligned bounding box with min and max points


-- expects p to be at the surface
getNormalAt :: Geometry -> Vector3 -> Vector3
getNormalAt (Sphere center _) p   = p .-. center
getNormalAt (Plane normal _) _    = normal
getNormalAt (Triangle p0 p1 p2) _ = vnormalize $ (p1 .-. p0) `vcross` (p2 .-. p0)

isGeometryOpen :: Geometry -> Bool
isGeometryOpen (Triangle _ _ _) = True
isGeometryOpen _        = False

-- convert barycentric coordinates u,v of a triangle to x,y,z
barycentric2world :: Vector3 -> Vector3 -> Vector3 -> Vector2 -> Vector3
barycentric2world p0 p1 p2 (Vector2 u v) =  (w *. p0) .+. (u *. p1) .+. (v *. p2)
  where w = (1 - u - v)

makeAABoxFromPoints :: [Vector3] -> Geometry
makeAABoxFromPoints points = AAbox bmin bmax                          
  where
    bmin = foldl1 (vvapply min) points
    bmax = foldl1 (vvapply max) points