import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Vector
import Utils

data Geometry = 
  Sphere Vector3 Float | -- center and radius
  Plane Vector3 Float    -- normal and constant


data Ray = Ray { origin    :: Vector3, direction :: Vector3 } deriving (Show)

data Object = Object {
  geometry :: Geometry,
  color :: Color,
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
  case filter (isFrontside) $rayIntersectionsWithinRange trange objects ray of
    []  -> Nothing
    lst -> Just $ minimumWith gett lst


colorMultiply :: Color -> Float -> Color
colorMultiply (Color r g b) s = Color (multc r) (multc g) (multc b)
  where 
    multc c = limits 0 1 c*s

traceRay :: World -> Ray -> (Float,Float) -> Color
traceRay world ray trange = 
  case pickObject (objects world) ray trange of
    Nothing -> backgroundColor world
    Just (object,(Frontside p _)) -> colorMultiply (color object) lightsItensity
      where
        lightsItensity = sum [ intensity | light@(Light _ _ intensity) <- (lights world), not (inShadowOf light)]

        inShadowOf :: Light -> Bool
        inShadowOf (Light lightPos _ _) = not $ isEmpty  $ rayIntersectionsWithinRange (0,distToLight) objects' lightray
          where 
            objects' = objects world
            dir = (lightPos .-. p)
            p0 =  p .+. (dir .* 0.001) --some tolerance to avoid  numerical problems
            lightray = makeRay p0 dir 
            distToLight = vmagnitude dir

          
--traceRay' :: World -> Ray -> Color
--traceRay' world ray =  
    
pixel2World :: Resolution -> Int -> Int -> (Float,Float,Float)
pixel2World (imWidth,imHeight) i j = toWorld ( toScreen ( toNDC ( toRaster i j) ) ) 
  where
    imWidth' = fromIntegral imWidth
    imHeight'= fromIntegral imHeight
    imAspRatio = imWidth'/imHeight'
    toRaster i j      = (fromIntegral j + 0.5, fromIntegral i + 0.5)
    toNDC    (x,y)    = (x/imWidth',y/imHeight')                 
    toScreen (nx,ny)  = ((2*nx - 1)*imAspRatio, - (2*ny - 1))    
    toWorld  (sx,sy)  =  (sx, sy, 1)

makeImage :: Resolution -> World -> [Color]
makeImage res@(width,height) world = [
  traceRay world (castRay $ pixel2World res i j ) (near,far) |
    i <- [1..height], 
    j <- [1..width]
  ]
  where
    castRay (a,b,c) = makeRay (Vector3 0 0 0) (Vector3 a b c)
    near = 0
    far = 1/0 --infinity

writePPM :: Resolution -> [Color] -> IO ()
writePPM (width,height) pixels  | width*height /= length pixels = error "Invalid width,height"
                                | otherwise = do
                                  BC.putStr $ header
                                  B.putStr $ body
  
  where
    header = BC.pack $ "P6\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body = B.pack $ concatMap color2ints pixels
    --body = show $ map color2ints pixels
    color2ints :: (Integral a, Num a, Ord a) => Color -> [a]
    color2ints (Color rf gf bf) = map ((max 0) . (min 255) . round . (*255)) [rf,gf,bf]
    
--main = print $ rayIntersect (makeRay (Vector3 0 0 0) (Vector3 0 0 10)) (Sphere (Vector3 0 0 10) 2)

myWorld = World {
  objects = [
    Object (Sphere (Vector3 0    0  20) 5 ) (Color 1 0.5 0.2) "sphere1",
    Object (Sphere (Vector3 0 (-30) 20) 20 ) (Color 1 0 0) "sphere2",
    Object (Sphere (Vector3 (-20) 0 20) 5 ) (Color 0 0.8 0.0) "sphere3",
    Object (Sphere (Vector3 0 40 100) 5 ) (Color 0 0.4 0.0) "sphere4",
    Object (Sphere (Vector3 5 5 50) 5 ) (Color 0.6 0.5 1.0) "sphere5",
    Object (Plane  (Vector3 0 1 0) (50) )  (Color 0 0 1) "plane"
  ],
  lights = [
    Light (Vector3 20 100 0) (Color 1 1 1) 0.5,
    Light (Vector3 (-20) 20 10) (Color 1 1 1) 0.5
    --Light (Vector3 0 0 0) (Color 1 1 1) 1
  ],
  backgroundColor = Color 1 1 1
}

myRes = (256,256)

myImage = makeImage myRes myWorld

main = writePPM myRes myImage
