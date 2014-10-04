module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Tracer
import Vector
import Ray
import Geometry
import Debug.Trace

pixel2World :: Resolution -> Int -> Int -> (Float,Float,Float)
pixel2World (imWidth,imHeight) i j = toWorld ( toScreen ( toNDC ( toRaster i j) ) ) 
  where
    imWidth' = fromIntegral imWidth
    imHeight'= fromIntegral imHeight
    imAspRatio = imWidth'/imHeight'
    toRaster i j      = (fromIntegral j + 0.5, fromIntegral i + 0.5)
    toNDC    (x,y)    = (x/imWidth',y/imHeight')                 
    toScreen (nx,ny)  = ((2*nx - 1)*imAspRatio, - (2*ny - 1))    
    toWorld  (sx,sy)  =  (sx, sy, -3)

makeImage :: Resolution -> World -> [Color]
makeImage res@(width,height) world = [
  traceRay world (castRay $ pixel2World res i j ) (near,far) 5 |
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
    header = trace ("maxc :" ++ show maxc) $ BC.pack $ "P6\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body = B.pack $ concatMap color2ints pixels
    maxc = maximum $ map (\(Color r g b) -> maximum [r,g,b]) pixels
    --body = show $ map color2ints pixels
    color2ints :: (Integral a, Num a, Ord a) => Color -> [a]
    color2ints (Color rf gf bf) = map ((max 0) . (min 255) . round . (*(255/maxc))) [rf,gf,bf]
    
--main = print $ rayIntersect (makeRay (Vector3 0 0 0) (Vector3 0 0 10)) (Sphere (Vector3 0 0 10) 2)

myWorld = World {
  objects = [
    --Object (Sphere (Vector3 (-6) 3  (-10)) 5 ) (Color 0.5 0.5 1) 1 1 "sphere1",
    --Object (Sphere (Vector3 5 2  (-13)) 5 ) (Color 0.8 0.5 0.5) 1 1 "sphere1b",
    --Object (Sphere (Vector3 0 (0) (-30)) 20 ) (Color 1 0.5 0.5) 1 1 "sphere2",
    --Object (Sphere (Vector3 (-20) 0 (-20)) 5 ) (Color 0 0.8 0.0) 1 1 "sphere3",
    --Object (Sphere (Vector3 0 40 (-100)) 5 ) (Color 0 0.4 0.0) 1 1 "sphere4",
    --Object (Sphere (Vector3 8 8 (-25)) 5 ) (Color 0.6 0.5 1.0) 1 1 "sphere5"
    --Object (Plane  (Vector3 0 1 0) (6) )  (Color 1 1 1) 1 0 "plane"
    --Object (Triangle (Vector3 0 0 (-20)) (Vector3 (10) 0 (-20)) (Vector3 (10) (10) (-20))) 
           --(Color 0.5 0.1 0.1) 0 0 "tri1"
    --Object (makeAABoxFromPoints [Vector3 (2) (-5) (-30), Vector3 (10) 5 (-25)]) (Color 0 1 0) 1 0 "box",
    --  --Object (Sphere (Vector3 0 (-10004) (-30)) 10000) (Color 0.2 0.2 0.2) 0 0 "c1",
    --  Object (Sphere (Vector3 0 0 (-20)) 4) (Color 1 0.32 0.36) 1 1 "c2",
    --  Object (Sphere (Vector3 5 (-1) (-15)) 2) (Color 0.90 0.76 0.46) 1 0.0 "c3",
    --  Object (Sphere (Vector3 5 (0) (-25)) 3) (Color 0.65 0.77 0.97) 1 0.0 "c4",
    --  Object (Sphere (Vector3 (-5.5) (0) (-15)) 3) (Color 0.90 0.90 0.90) 1 0.0 "c5",
      Object (UnitCylinder (Vector3 0 0 (-40))) (Color 1 0 0) 0 0 "cyl"
  ],
  lights = [
    --Light (Vector3 20 100 0) (Color 1 1 1) 0.5
    Light (Vector3 (0) 20 (-30)) (Color 1 1 1) 1
    --Light (Vector3 0 0 0) (Color 1 1 1) 1
  ],
  backgroundColor = Color 1 1 1
}

myRes = (500,500)

myImage = makeImage myRes myWorld

main = writePPM myRes myImage
--tri = (Triangle (Vector3 0 0 20) (Vector3 (-10) 0 20) (Vector3 (-10) 10 20)) 
--main = print $ rayIntersect (makeRay (Vector3 0 0 0) (Vector3 (-5) 5 20)) tri
--box = AABox (Vector3 (0) (9) (0)) (Vector3 1 10 1)
--main = print $ rayIntersect (makeRay (Vector3 0 0 0) (Vector3 (-0) 1 (-0))) box
--castRay (a,b,c) = makeRay (Vector3 0 0 0) (Vector3 a b c)
--main = print $ traceRayDbg myWorld (castRay $ (0,0,3) ) (0,1/0) 10