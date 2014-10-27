module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Tracer
import Vector
import Ray
import Geometry
import Utils
import Object
import Color
import World
import Light
--import Graphene

cameraOrigin = Vector3 0 0 0
maxdepth = 5

type Resolution = (Int, Int)

pixel2World :: Resolution -> Int -> Int -> (Float,Float,Float)
pixel2World (imWidth,imHeight) i j = toWorld ( toScreen ( toNDC ( toRaster i j) ) ) 
  where
    imWidth' = fromIntegral imWidth
    imHeight'= fromIntegral imHeight
    imAspRatio = imWidth'/imHeight'
    toRaster i j      = (fromIntegral j + 0.5, fromIntegral i + 0.5)
    toNDC    (x,y)    = (x/imWidth',y/imHeight')                 
    toScreen (nx,ny)  = ((2*nx - 1)*imAspRatio, - (2*ny - 1))    
    toWorld  (sx,sy)  =  (sx,sy, -1)

makeImage :: Resolution -> World -> [Color]
makeImage res@(width,height) world = [
  traceRay world (castRay $ pixel2World res i j ) (near,far) maxdepth |
    i <- [1..height], 
    j <- [1..width]
  ]
  where
    castRay (a,b,c) = makeRay cameraOrigin (Vector3 a b c)
    near = 0
    far = 1/0 --infinity

writePPM :: Resolution -> [Color] -> IO ()
writePPM (width,height) pixels  | width*height /= length pixels = error "Invalid width,height"
                                | otherwise = do
                                  BC.putStr $ header
                                  B.putStr $ body
  
  where
    header = BC.pack $ "P6\n" ++ show width ++ " " ++ show height ++ "\n255\n"  `debug` ("maxc :" ++ show maxc)
    body = B.pack $ concatMap color2ints pixels
    maxc = maximum $ map (\(Color r g b) -> maximum [r,g,b]) pixels
    --body = show $ map color2ints pixels
    color2ints :: (Integral a, Num a, Ord a) => Color -> [a]
    color2ints (Color rf gf bf) = map ((max 0) . (min 255) . round . (*(255/maxc))) [rf,gf,bf]
    
--main = print $ rayIntersect (makeRay (Vector3 0 0 0) (Vector3 0 0 10)) (Sphere (Vector3 0 0 10) 2)

#include "examples/spheres.hs"

myRes = (500,500)

myImage = makeImage myRes myWorld

main = writePPM myRes myImage
--tri = (Triangle (Vector3 0 0 20) (Vector3 (-10) 0 20) (Vector3 (-10) 10 20)) 
--main = print $ rayIntersect (makeRay (Vector3 0 0 0) (Vector3 (-5) 5 20)) tri
--box = AABox (Vector3 (0) (9) (0)) (Vector3 1 10 1)
--main = print $ rayIntersect (makeRay (Vector3 0 0 0) (Vector3 (-0) 1 (-0))) box
--castRay (a,b,c) = makeRay (Vector3 0 0 0) (Vector3 a b c)
--main = print $ traceRayDbg myWorld (castRay $ (0,0,3) ) (0,1/0) 10