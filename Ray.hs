module Ray (module Ray) where

import Vector
import Utils
import Geometry
import Matrix
import Object
import Color

data Ray = Ray { origin    :: Vector3, direction :: Vector3 } deriving (Show)

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

rayIntersect ray@(Ray o d) cyl@(InfiniteCylinder cc r)
  | isNothing roots   = None -- no intersection
  | t0 < 0 && t1 < 0  = None
  | t0 < 0 && t1 >= 0 = Backside (rayPointAt ray t1) t1
  | otherwise         = Frontside (rayPointAt ray t0) t0

  where
    (Vector3 dx _ dz) = d
    (Vector3 ox _ oz) = o .-. cc --translates origin to cylinder coordinates

    a = dx*dx + dz*dz
    b = 2*(dx*ox + dz*oz)
    c = ox*ox + oz*oz - r
    roots = rootspoly2 a b c --`debug` (show (a,b,c))
    (t0,t1) = sortTuple2 $ getSomething roots

--rayIntersect ray@(Ray o d) cyl@(Cylinder co cd amin amax r) =
--  case rayIntersect (Ray o d') (InfiniteCylinder co r) of 
--    None -> None
--    (Backside  p' t) -> if withinRange p' then Backside  (p' .+. co) t else None
--    (Frontside p' t) -> if withinRange p' then Frontside (p' .+. co) t else None

--  where
--    --get rotation transform from cylinder axis to +y
--    rotAxis = cd `vcross` (Vector3 0 1 0)
--    rotCosAngle = cd `vdot` (Vector3 0 1 0)
--    rotMatrix = rotationAroundAxisMatrix3 rotAxis rotCosAngle
--    --apply transform to ray
--    d' = matMultVector3 rotMatrix d
--    Vector3 _ coy _ = co

--    withinRange (Vector3 _ y _) = (y + coy) > amin && (y + coy) < amax

rayIntersect ray@(Ray o d) cyl@(Cylinder co cd hmin hmax r)
  | isNothing roots                 = None -- no intersection
  | t0 < 0 && t1 < 0                = None -- facing away
  | h0 < hmin && h1 < hmin          = None
  | h0 > hmax && h1 > hmax          = None
  | h0 < hmin                       = rayIntersect ray bottomCap --`debug` "bottom"
  | h0 > hmax                       = rayIntersect ray topCap --`debug` "top"
  | t0 < 0 && t1 >= 0               = Backside (rayPointAt ray t1) t1
  | otherwise                       = Frontside (rayPointAt ray t0) t0
  where
    --don't assume d is normalized, just in case we did some transform to get here
    a = (d `vdot` d) - (d `vdot` cd)^2
    b = 2*( (l `vdot` d) - (l `vdot` cd)*(d `vdot` cd))
    c = (l `vdot` l) - (l `vdot` cd)^2 - r^2
    l = o .-. co
    roots = rootspoly2 a b c --`debug` (show (a,b,c))
    (t0,t1) = sortTuple2 $ getSomething roots

     --get position in cylinder axis (relative to co)
    calch t = (l `vdot` cd) + (d `vdot` cd)*t
    --calch t = ((rayPointAt ray t) .-. co) `vdot` cd
    (h0,h1) = (calch t0, calch t1)

    outOfRange h = h < hmin || h > hmax -- `debug` (show h)
    inRange = not . outOfRange

    axisRay@(Ray _ cd') = makeRay co cd --also normalizes cd just in case
    bottomCap = Disk (rayPointAt axisRay hmin) (vnegate cd') r
    topCap    = Disk (rayPointAt axisRay hmax) cd' r

-- ray -disk intersection
rayIntersect ray@(Ray o d) disk@(Disk c n r) =
  case rayIntersect ray (makePlaneFromPointAndNormal c n) of 
    None -> None
    intp -> if (vmagnitude (p .-. c)) < r then intp else None
      where
        p = getp' intp
           
rayIntersections :: [Object] -> Ray -> [(Object,Intersection)]
rayIntersections []   _     = []
rayIntersections (o:os) ray = (rayIntersect' o) ++ (rayIntersections os ray)
  where
    rayIntersect' obj = 
      case rayIntersectObject ray obj of None                -> []
                                         int@(Backside  _ _) -> [(obj,int)]
                                         int@(Frontside _ _) -> [(obj,int)]


rayIntersectObject :: Ray -> Object -> Intersection
rayIntersectObject ray obj = rayIntersect ray (geometry obj)

rayIntersectionsWithinRange :: (Float, Float) -> [Object] -> Ray -> [(Object,Intersection)]
rayIntersectionsWithinRange (tmin,tmax) objs ray = (filter (withinRange tmin tmax . gett)) $ rayIntersections objs ray
  
gett' :: Intersection -> Float
gett' (Frontside _ t) = t
gett' (Backside _ t) = t

gett (_,i) = gett' i

getp' :: Intersection -> Vector3
getp' (Frontside p _) = p
getp' (Backside p _) = p

getp (_,i) = getp' i

isFrontside' :: Intersection -> Bool
isFrontside' (Frontside _ _) = True
isFrontside' (Backside _ _ ) = False
isFrontside (_,i) = isFrontside' i

isBackside' :: Intersection -> Bool
isBackside' (Frontside _ _) = False
isBackside' (Backside _ _ ) = True
isBackside (_,i) = isBackside' i

isNone :: Intersection -> Bool
isNone None = True
isNone _ = False



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


