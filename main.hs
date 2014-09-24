import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Tracer
import Vector
import Ray

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

myRes = (500,500)

myImage = makeImage myRes myWorld

main = writePPM myRes myImage