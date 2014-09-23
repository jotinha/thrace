import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr)
import Core

data Geometry = Sphere { center :: Vector, radius :: Float }

data Ray = Ray { origin    :: Vector, direction :: Vector } deriving (Show)

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

data Light = Light Vector Color Float --position color intensity

type Resolution = (Int, Int)

data Intersection = None | Backside Vector Float | Frontside Vector Float deriving (Eq)

makeRay :: Vector -> Vector -> Ray
makeRay o d = Ray o d' where d' = vnormalize d

rayPointAt :: Ray -> Float -> Vector
rayPointAt (Ray o d) t = o `vplus` (d `smult` t)

rayIntersect :: Ray -> Geometry -> Intersection
rayIntersect ray@(Ray o d) (Sphere c r)   | s < 0 && l2 > r2  = None
                                          | l2 <= r2 = Backside p t
                                          | m2 > r2 = None
                                          | otherwise =  Frontside p t
  where 
    l = c `vminus` o
    s = l `vdot` d
    r2 = r * r
    l2 = vsq l
    s2 = s * s
    m2 = l2 - s2
    q = sqrt (r2 - m2)
    t0 = s - q
    t1 = s + q
    t = if t0 > 0 then t0 else t1
    p = rayPointAt ray t

rayIntersections :: [Object] -> Ray -> [(Object,Intersection)]
rayIntersections []   _     = []
rayIntersections (o:os) ray = (rayIntersect' o) ++ (rayIntersections os ray)
  where
    rayIntersect' obj = 
      case rayIntersect ray (geometry obj) of None                -> []
                                              int@(Backside  _ _) -> [(obj,int)]
                                              int@(Frontside _ _) -> [(obj,int)]


rayFrontIntersections :: [Object] -> Ray -> [(Object,Intersection)]
rayFrontIntersections objects ray = filter isFrontSide  (rayIntersections objects ray)
  where
    isFrontSide :: (Object,Intersection) -> Bool
    isFrontSide (_,(Frontside _ _)) = True
    isFrontSide (_,(Backside  _ _)) = False


pickObject :: [Object] -> Ray -> Maybe (Object,Intersection)
pickObject  objects ray =
  case rayFrontIntersections objects ray of
    []  -> Nothing
    lst -> Just $ minimumWith (\(_,(Frontside _ t)) -> t) lst

colorMultiply :: Color -> Float -> Color
colorMultiply (Color r g b) s = Color (multc r) (multc g) (multc b)
  where 
    multc c = limits 0 1 c*s

traceRay :: World -> Ray -> Color
traceRay world ray = 
  case pickObject (objects world) ray of
    Nothing -> backgroundColor world
    Just (object,(Frontside p _)) -> colorMultiply (color object) lightsItensity
      where
        lightsItensity = sum [ intensity | light@(Light _ _ intensity) <- (lights world), not (inShadowOf light)]

        inShadowOf :: Light -> Bool
        inShadowOf (Light lightPos _ _) = length (rayIntersections objects' lightray) > 0
          where 
            objects' = objects world
            dir = (lightPos `vminus` p)
            p0 =  p `vplus` (dir `smult` 0.001) --some tolerance to avoid  numerical problems
            lightray = makeRay p0 dir 

          
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
  traceRay world $ castRay $ pixel2World res i j |
    i <- [1..height], 
    j <- [1..width]
  ]
  where
    castRay (a,b,c) = makeRay (Vector 0 0 0) (Vector a b c)

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
    
--main = print $ rayIntersect (makeRay (Vector 0 0 0) (Vector 0 0 10)) (Sphere (Vector 0 0 10) 2)

myWorld = World {
  objects = [
    Object (Sphere (Vector 0    0  20) 5 ) (Color 1 0.5 0.2) "sphere1",
    Object (Sphere (Vector 0 (-30) 20) 20 ) (Color 1 0 0) "sphere2",
    Object (Sphere (Vector (-20) 0 20) 5 ) (Color 0 0.8 0.0) "sphere3",
    Object (Sphere (Vector 0 40 100) 5 ) (Color 0 0.4 0.0) "sphere4",
    Object (Sphere (Vector 5 5 50) 5 ) (Color 0.6 0.5 1.0) "sphere5"
  ],
  lights = [
    Light (Vector 20 100 0) (Color 1 1 1) 0.5,
    Light (Vector (-20) 20 10) (Color 1 1 1) 0.5
    --Light (Vector 0 0 0) (Color 1 1 1) 1
  ],
  backgroundColor = Color 1 1 1
}

myRes = (500,500)

myImage = makeImage myRes myWorld

main = writePPM myRes myImage
