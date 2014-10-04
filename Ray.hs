module Ray (module Ray) where

import Vector
import Utils
import Geometry

data Ray = Ray { origin    :: Vector3, direction :: Vector3 } deriving (Show)

data Object = Object {
  geometry      :: Geometry,
  color         :: Color,
  reflection    :: Float,
  transparency  :: Float,
  objId :: String
}

instance Eq Object where
  a == b = (objId a) == (objId b)

data Color = Color Float Float Float deriving (Show)

data World = World { 
  objects :: [Object],
  lights  :: [Light],
  backgroundColor :: Color
}

data Light = Light Vector3 Color Float --position color intensity

type Resolution = (Int, Int)

data Intersection = None | Backside Vector3 Float | Frontside Vector3 Float deriving (Eq,Show)

makeRay :: Vector3 -> Vector3 -> Ray
makeRay o d = Ray o d' where d' = vnormalize d

castRayFromTowards :: Vector3 -> Vector3 -> Ray
castRayFromTowards a b = makeRay a (b .-. a)

rayPointAt :: Ray -> Float -> Vector3
rayPointAt (Ray o d) t = o .+. (d .* t)

rayIntersect :: Ray -> Geometry -> Intersection

--Ray - Sphere intersection
rayIntersect ray@(Ray o d) (Sphere c r)   | s < 0 && l2 > r2  = None
                                          | l2 <= r2 = Backside p t
                                          | m2 > r2 = None
                                          | otherwise =  Frontside p t
  where 
    l = c .-. o
    s = l `vdot` d
    r2 = r * r
    l2 = l `vdot` l
    s2 = s * s
    m2 = l2 - s2
    q = sqrt (r2 - m2)
    t0 = s - q
    t1 = s + q
    t = if t0 > 0 then t0 else t1
    p = rayPointAt ray t

-- Ray - Plane intersection
rayIntersect ray@(Ray o d) (Plane n const)  | ndist == 0  = Backside o 0  --origin of ray is in plane
                                            | den == 0    = None          --parallel to plane (since ndist != 0 it's not coplanar)
                                            --TODO: include tol
                                            | t <  0      = None          --ray facing away from plane
                                            | ndist <  0  = Backside p t  --origin behind plane
                                            | ndist > 0   = Frontside p t --origin in front of plane
  where
    ndist = (n `vdot` o) + const   --normal distance from origin to plane
    den = n `vdot` d
    t = - (ndist / den)
    p = rayPointAt ray t    

-- Ray - Triangle intersection
rayIntersect ray@(Ray o d) (Triangle p0 p1 p2)  | a > -tol && a < tol = None  -- parallel
                                                | u < 0 || u > 1      = None
                                                | v < 0 || u + v > 1  = None
                                                | a > 0               = Frontside p t
                                                | a < 0               = Backside p t

  where
    e1 = p1 .-. p0
    e2 = p2 .-. p0
    q = d `vcross` e2
    a = e1 `vdot` q -- same as -n.d (where n is unormalized normal e1 x e2)
    s = o .-. p0
    u = (s `vdot` q) / a
    r = s `vcross` e1
    v = (d `vdot` r) / a
    t = (e2 `vdot` r) / a  -- the [Realtime -rendering 2nd ed] book has it wrong here
    p = barycentric2world p0 p1 p2 (Vector2 u v)
    tol = 0.001

rayIntersect ray@(Ray o d) (AABox bmin bmax) 
  | txmin > tymax || txmax < tymin = None
  | tmin' > tzmax || tmax' < tzmin = None
  | tmax < 0 = None
  | tmin < 0 && tmax < 0  = error "both tmin and tmax are negative"
  | tmin > tmax  = error "tmin > tmax" --`debug` (show (a,b))
  | tmin < 0   = Backside  (rayPointAt ray tmax) tmax
  | otherwise  = Frontside (rayPointAt ray tmin) tmin


  where
    bound1 = vvvApply (\a b c -> if 1/a >= 0 then b else c) d bmin bmax
    bound2 = vvvApply (\a b c -> if 1/a >= 0 then b else c) d bmax bmin
    a@(Vector3 txmin tymin tzmin) = (bound1 .-. o) ./. d
    b@(Vector3 txmax tymax tzmax) = (bound2 .-. o) ./. d
    tmin' = max txmin tymin
    tmax' = min txmax tymax
    tmin = max tmin' tzmin
    tmax = min tmax' tzmax

    vvvApply :: (Float -> Float -> Float -> Float) -> Vector3 -> Vector3 -> Vector3 -> Vector3
    vvvApply f (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) (Vector3 x3 y3 z3) = Vector3 (f x1 x2 x3) (f y1 y2 y3) (f z1 z2 z3)


-- Ray - AABox intersection
--rayIntersect ray@(Ray o d) (AABox bmin bmax) 
--  | t0x > t1y || t0x > t1z || t0y > t1x || t0y > t1z ||t0z > t1x || t0z > t1z = None
--  | tmin < 0 && tmax < 0  = error "both tmin and tmax are negative"
--  | tmin > tmax  = error "tmin > tmax" `debug` (show (bound1,bound2))
--  | tmin < 0   = Backside  (rayPointAt ray tmax) tmax
--  | otherwise  = Frontside (rayPointAt ray tmin) tmin

--  where
--    bound1 = vvvApply (\a b c -> if 1/a >= 0 then b else c) d bmin bmax
--    bound2 = vvvApply (\a b c -> if 1/a >= 0 then b else c) d bmax bmin
--    (Vector3 t0x t0y t0z)  = (bound1 .-. o) ./. d
--    (Vector3 t1x t1y t1z)  = (bound2 .-. o) ./. d
--    tmin = maximum [t0x,t0y,t0z]
--    tmax = minimum [t1x,t1y,t1z]
    
--    vvvApply :: (Float -> Float -> Float -> Float) -> Vector3 -> Vector3 -> Vector3 -> Vector3
--    vvvApply f (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) (Vector3 x3 y3 z3) = Vector3 (f x1 x2 x3) (f y1 y2 y3) (f z1 z2 z3)

rayIntersect ray@(Ray o d) unitcyl@(UnitCylinder cc)
  | isNothing roots   = None -- no intersection
  | t0 < 0 && t1 < 0  = None
  | t0 < 0 && t1 >= 0 = Backside (rayPointAt ray t1) t1
  | otherwise         = Frontside (rayPointAt ray t0) t0

  where
    (Vector3 dx _ dz) = d
    (Vector3 ox _ oz) = o .-. cc --translates origin to cylinder coordinates

    a = dx*dx + dz*dz
    b = 2*(dx*ox + dz*oz)
    c = ox*ox + oz*oz - 1
    roots = rootspoly2 a b c --`debug` (show (a,b,c))
    (t0,t1) = sortTuple2 $ getSomething roots


rayIntersections :: [Object] -> Ray -> [(Object,Intersection)]
rayIntersections []   _     = []
rayIntersections (o:os) ray = (rayIntersect' o) ++ (rayIntersections os ray)
  where
    rayIntersect' obj = 
      case rayIntersect ray (geometry obj) of None                -> []
                                              int@(Backside  _ _) -> [(obj,int)]
                                              int@(Frontside _ _) -> [(obj,int)]

rayIntersectionsWithinRange :: (Float, Float) -> [Object] -> Ray -> [(Object,Intersection)]
rayIntersectionsWithinRange (tmin,tmax) objs ray = (filter (withinRange tmin tmax . gett)) $ rayIntersections objs ray
  
gett :: (Object,Intersection) -> Float
gett (_,(Frontside _ t)) = t
gett (_,(Backside _ t))  = t

getp :: (Object,Intersection) -> Vector3
getp (_,(Frontside p _)) = p
getp (_,(Backside p _))  = p

isFrontside :: (Object,Intersection) -> Bool
isFrontside (_,(Frontside _ _)) = True
isFrontside (_,(Backside  _ _)) = False

isBackside :: (Object,Intersection) -> Bool
isBackside (_,(Frontside _ _)) = False
isBackside (_,(Backside  _ _)) = True


pickObject :: [Object] -> Ray -> (Float,Float) -> Maybe (Object,Intersection)
pickObject  objects ray trange =
  case filter (isFrontside) $ rayIntersectionsWithinRange trange objects ray of
    []  -> Nothing
    lst -> Just $ minimumWith gett lst

pickObjectAllowBackside :: [Object] -> Ray -> (Float,Float) -> Maybe (Object,Intersection)
pickObjectAllowBackside  objects ray trange =
  case rayIntersectionsWithinRange trange objects ray of
    []  -> Nothing
    lst -> Just $ minimumWith gett lst
