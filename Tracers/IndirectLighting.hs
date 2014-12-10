module Tracers.IndirectLighting (traceRay) where
-- Diffuse indirect lighting only

import Ray
import Utils
import Vector
import Geometry
import Color
import Object
import Light
import World
import Physics
import Data.Tuple (swap)
import Random
import Material
import Tracers.Common
import Config

traceRay :: World -> Ray -> (Float,Float) -> Int -> Color
traceRay world ray trange maxdepth
  | maxdepth <= 0           = white -- maxdepth is necessary again because we are casting diffuseRays
  | isNothing intersection' = backgroundColor world
  | maxdepth ==  1          = localColor 
  | otherwise               = localColor `colorAdd` indirectColor
  where

    intersection' = pickObjectAllowBackside (objects world) ray trange
    intersection = (\(Just x) -> x) intersection'   --cast down from Maybe a
    p = getp intersection
    object = fst intersection
    isInside = isBackside intersection

    matBRDF = brdf (material object)

    v = vnegate $ direction ray
    n = surfNormal

    localColor = localIllumination world v p n matBRDF numberShadowRays False

    --cast global illumination rays - one bounce only
    seed = generateSeed [0] [p, v]
    diffuseRays = take numberDiffuseRays $
                  map (spawnRay p) $
                  filter (isPositiveHalf) $ 
                  randomPointsOnSurfaceOf (Sphere (Vector3 0 0 0) 1) seed
      where
        isPositiveHalf v = n `vdot` v > 0

    indirectColor = colorAverage $ map (\diffuseRay -> traceRay world diffuseRay trange 1) diffuseRays


    incidentDir = (\(Ray _ d) -> d) ray
    surfNormal  = getNormalAt (geometry object) p
    surfNormal' = if isInside then (vnegate surfNormal) else surfNormal
    

