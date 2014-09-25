module Ray (module Ray) where

import Vector
import Utils


data Geometry = 
  Sphere Vector3 Float | -- center and radius
  Plane Vector3 Float    -- normal and constant

-- expects p to be at the surface
getNormalAt :: Geometry -> Vector3 -> Vector3
getNormalAt (Sphere center _) p  = p .-. center
getNormalAt (Plane normal _) _   = normal

data Ray = Ray { origin    :: Vector3, direction :: Vector3 } deriving (Show)

data Object = Object {
  geometry      :: Geometry,
  color         :: Color,
  transparency  :: Float,
  reflection    :: Float,
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

data Intersection = None | Backside Vector3 Float | Frontside Vector3 Float deriving (Eq)

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
                                            | t <  0      = None          --ray facing away from plane
                                            | ndist <  0  = Backside p t  --origin behind plane
                                            | ndist > 0   = Frontside p t --origin in front of plane
  where
    ndist = (n `vdot` o) + const   --normal distance from origin to plane
    den = n `vdot` d
    t = - (ndist / den)
    p = rayPointAt ray t    

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
