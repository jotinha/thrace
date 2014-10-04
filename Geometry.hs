module Geometry (module Geometry) where

import Vector
import Utils

data Geometry 
  = Sphere Vector3 Float -- center and radius
  | Plane Vector3 Float -- normal and constant
  | Triangle Vector3 Vector3 Vector3
  | AABox Vector3 Vector3  --axis aligned bounding box with min and max points
  | InfiniteCylinder Vector3 Float    -- infinite cylinder with main axis in y direction 
                                      -- with args : position  radius



-- expects p to be at the surface
getNormalAt :: Geometry -> Vector3 -> Vector3
getNormalAt (Sphere center _) p       = vnormalize $ p .-. center
getNormalAt (Plane normal _) _        = normal
getNormalAt (Triangle p0 p1 p2) _     = vnormalize $ (p1 .-. p0) `vcross` (p2 .-. p0)
getNormalAt (InfiniteCylinder cc _) p = vnormalize $ (p .-. cc) .*. (Vector3 1 0 1) --this discards the y coordinate

getNormalAt (AABox (Vector3 minx miny minz) (Vector3 maxx maxy maxz)) (Vector3 px py pz)
  | px <= minx + tol = Vector3 (-1) 0 0
  | px >= maxx - tol = Vector3 1 0 0
  | py <= miny + tol = Vector3 0 (-1) 0
  | py >= maxy - tol = Vector3 0 1 0
  | pz <= minz + tol = Vector3 0 0 (-1)
  | pz >= maxz - tol = Vector3 0 0 1
  where
    tol = 0.0001
--getNormalAt (AABox (Vector3 minx miny minz) (Vector3 maxx maxy maxz)) (Vector3 px py pz) =
--  snd $ minimumWith (abs . fst) [
--    (px - minx, Vector3 (-1) 0 0  ),
--    (px - maxx, Vector3 1 0 0     ),
--    (py - miny, Vector3 0 (-1) 0  ),
--    (py - maxy, Vector3 0 1 0     ),
--    (pz - minz, Vector3 0 0 (-1)  ),
--    (pz - maxz, Vector3 0 0 1     )
--  ]



isGeometryOpen :: Geometry -> Bool
isGeometryOpen (Triangle _ _ _) = True
isGeometryOpen _        = False

-- convert barycentric coordinates u,v of a triangle to x,y,z
barycentric2world :: Vector3 -> Vector3 -> Vector3 -> Vector2 -> Vector3
barycentric2world p0 p1 p2 (Vector2 u v) =  (w *. p0) .+. (u *. p1) .+. (v *. p2)
  where w = (1 - u - v)

makeAABoxFromPoints :: [Vector3] -> Geometry
makeAABoxFromPoints points = AABox bmin bmax                          
  where
    bmin = foldl1 (vvapply min) points
    bmax = foldl1 (vvapply max) points