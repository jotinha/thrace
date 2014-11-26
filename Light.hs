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

raysToLight :: LightType -> Vector3 -> Maybe (Int,Seed) -> [Vector3]
raysToLight (PointLight o) p _ = [o .-. p]
raysToLight (DirectionalLight d) _ _ = [vnegate d] --should multiply by Inf?
raysToLight (GeomLight geo) p Nothing      = [(centerOf geo) .-. p]
raysToLight (GeomLight geo) p (Just (1,_)) = [(centerOf geo) .-. p]
raysToLight (GeomLight geo) p (Just (n,g)) = take n $
  [v .-. p | v <- randomPointsOnSurfaceOf geo g]
  where
    isPositiveSide v = ((v .-. p) `vdot` (v .-. c)) <= 0
    first = c .-. p -- use this as first ray?
    c = centerOf geo


