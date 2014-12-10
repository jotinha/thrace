module Tracers.Basic (traceRay) where

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
traceRay world ray trange _
  | isNothing intersection' = backgroundColor world
  | otherwise               = localColor
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

    incidentDir = (\(Ray _ d) -> d) ray
    surfNormal  = getNormalAt (geometry object) p
    surfNormal' = if isInside then (vnegate surfNormal) else surfNormal
    
