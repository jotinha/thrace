module Random where

import Vector
import Geometry
import System.Random  
import Data.Hashable
import Utils

type Seed = StdGen

randomPointsOnSurfaceOf :: Geometry -> Seed -> [Vector3]
randomPointsOnSurfaceOf (Sphere o r) g = 
  map fromUniSphere $ randomRemapFromCanonical2 randUniSphere g
    where
      fromUniSphere u = o .+. (u .* r)
      randUniSphere (r0,r1) = Vector3 x y z
        where
          z = 2 * r0 -1
          s = sqrt $ 1 - z*z
          t = (2 * r1 - 1) * (2*pi)
          x = s*(cos t)
          y = s*(sin t)

-- uses a better sampling distribution than uniform
-- Ref [FCG pp 631-633]
-- Arguments:
  -- sphere : Luminaire geometry, must be sphere
  -- x : point on incident surface
-- Outputs: [x',p(x')] 
  -- x' : Point on the surface
  -- p(x'): Probability of this point

--randomPointsOnSphericalLuminaire :: Geometry -> Vector3 -> Seed -> [(Vector3,Float)]
--randomPointsOnSphericalLuminaire sphere@(Sphere c r) x g = 
--  tail $ [ (x',p_x') | ((x',p_x'),_) <- iterate randomPoint ((Vector3 0 0 0,0), g)]
--  where
--    randomPoint (_,g0) = ((x',p x'), g2)
--      where
--        --max radius
--        d = c .-. x
--        cos_alpha_max = sqrt (1 - r*r/(vsq d))
        
--        --define orhonormal basis uvw
--        n = if abs (up `vdot` w) > tol then up else fwd
--        tol = 1e-2

--        w = vnormalize $ c .-. x
--        v = vnormalize $ w `vcross` n
--        u = vnormalize $ v `vcross` w

--        --compute perturbed ray
--        k_i = (u .* ((cos phi)*sin_alpha)) .+. 
--              (v .* ((sin phi)*sin_alpha)) .+. 
--              (w .* cos_alpha)

--        -- intersection of ray with sphere gets the point
--        x' = case rayIntersect (Ray x k_i) sphere of 
--              Frontside p _ -> p
--              otherwise     -> error "expected frontside interscection with sphere luminaire"

--        -- probability distributions
--        q = 1 / (2*pi*(1 - cos_alpha_max))
--        p x' = cos_alpha' * q / dist2
--          where
--            dist2 = vsq $ x' .-. x
--            a = vnormalize $ x' .-. c
--            cos_alpha' = - (a `vdot` (vnormalize d))

--        --random variables
--        cos_alpha = 1 - u0 + u0 * cos_alpha_max
--        phi       = 2*pi*u1
--        (u0,g1) = random g0
--        (u1,g2) = random g1
--        sin_alpha = sqrt $ 1 - cos_alpha*cos_alpha


generateSeed :: [Int] -> [Vector3] -> Seed
generateSeed xs vs = mkStdGen $ hash $ (map toEnum xs) ++ (concatMap vToList vs)


-- generates pairs of uniform random numbers, applies function f to them to remap random variables
randomRemapFromCanonical2 :: Random a => ((a,a) -> t) -> Seed -> [t]
randomRemapFromCanonical2 f g = map f $ takeConsecutivePairs $ randoms g
