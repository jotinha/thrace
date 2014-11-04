module Random where

import Vector
import Geometry
import System.Random  
import Data.Hashable

type Seed = StdGen

randomPointsInSurfaceOf :: Geometry -> Seed -> [Vector3]
randomPointsInSurfaceOf (Sphere o r) g = 
  [o .+. (u .* r) | (u,_) <- iterate randUniSphere (Vector3 0 0 0, g)]

  where
    randUniSphere (_,g0) = (Vector3 x y z,g2)
      where (r0,g1) = random g0
            (r1,g2) = random g1
            z = 2 * r0 -1
            s = sqrt $ 1 - z*z
            t = (2 * r1 - 1) * (2*pi)
            x = s*(cos t)
            y = s*(sin t)


generateSeed :: [Int] -> [Vector3] -> Seed
generateSeed xs vs = mkStdGen $ hash $ (map toEnum xs) ++ (concatMap vToList vs)
