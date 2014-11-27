module Light where

import Vector
import Color
import Geometry
import Random

data LightType 
  = PointLight Vector3 --position
  | DirectionalLight Vector3 -- direction
  | GeomLight Geometry
  deriving (Show,Eq)


data Light = Light LightType Color deriving (Show, Eq)

lightFalloff :: LightType -> Float -> Float
lightFalloff (DirectionalLight _) _ = 1
lightFalloff _ r = 1.0/r/r

lightPos :: LightType -> Vector3
lightPos (PointLight o) = o
lightPos (DirectionalLight _) = (Vector3 1 1 1) ./ 0
lightPos (GeomLight geo) = centerOf geo

--returns a list of pairs of vectors l and float cos_l where
--l is the vector from p to a point in the light surface (unormalized)
--and cos_l = -n.l the dot product between the vector l and normal at the
--surface point
raysToLight :: LightType -> Vector3 -> Maybe (Int,Seed) -> [(Vector3,Float)]
raysToLight (PointLight o) p _ = [(o .-. p,1)]
raysToLight (DirectionalLight d) _ _ = [(vnegate d,1)] --should multiply by Inf?
raysToLight (GeomLight geo) p Nothing      = [((centerOf geo) .-. p,1)]
raysToLight (GeomLight geo) p (Just (1,_)) = [((centerOf geo) .-. p,1)]
raysToLight (GeomLight geo) p (Just (n,g)) = take n $
  [(v, cos_l ) | 
    s <- randomPointsOnSurfaceOf geo g, 
    let v = s .-. p,
    let n = getNormalAt geo s,
    let cos_l = - ((vnormalize v) `vdot` n), --v and n are facing each other, invert
    cos_l > 0 ] -- cos_l > 0 selects only positive half sides


