module Light where

import Vector
import Color
import Geometry

data LightType 
  = PointLight Vector3 --position
  | DirectionalLight Vector3 -- direction
  | GeomLight Geometry

data Light = Light LightType Color

lightFalloff :: LightType -> Float -> Float
lightFalloff (DirectionalLight _) _ = 1
lightFalloff _ r = 1.0/r/r


raysToLight :: LightType -> Vector3 -> [Vector3]
raysToLight (PointLight o) p = [o .-. p]
raysToLight (DirectionalLight d) _ = [vnegate d]
raysToLight (GeomLight (Sphere o r)) p = [o .-. p] --TODO sample multiple rays
raysToLight _ _ = error "not implemented"

