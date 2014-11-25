module Tracers.SimpleRecursive (traceRay) where

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
import Data.List (elemIndex)
import Random
import Material
import Tracers.Common

--only purely specular (mirror) reflections and refractions
traceRay :: World -> Ray -> (Float,Float) -> Int -> Color
traceRay world ray trange maxdepth
  | maxdepth <= 0           = white
  | isNothing intersection' = backgroundColor world
  | otherwise               = colorDiffuse `colorAdd` colorMix

  where

    numberShadowRays = 1

    intersection' = pickObjectAllowBackside (objects world) ray trange
    intersection = (\(Just x) -> x) intersection'   --cast down from Maybe a
    p = getp intersection
    object = fst intersection
    isInside = isBackside intersection

    --we don't want specular highlights, so we use the diffuse component of the BRDF only
    matBRDF _ _ _ = getDiffuseComponent (material object)
    v = vnegate $ direction ray
    n = surfNormal
    colorDiffuse = localIllumination world v p n matBRDF numberShadowRays allowTransparentShadows
               
    -- Cast reflection ray ------------------------------------------------------------------------
    colorReflection   = traceRay world (spawnRay p reflectionvector) trange (maxdepth -1)
                        
    -- Cast refraction ray ------------------------------------------------------------------------
    colorRefraction   = traceRay world (spawnRay p refractionvector) trange (maxdepth -1)

    -- Mix refraction and reflection -------------------------------------------------------------
    colorMix = if reflectionAmount > 0 || refractionAmount > 0
               then (colorMultiplyScalar colorReflection $ reflectionAmount / (reflectionAmount + refractionAmount)) 
                      `colorAdd` 
                    (colorMultiplyScalar colorRefraction $ refractionAmount / (reflectionAmount + refractionAmount)) 
               else black
    
    reflectionAmount  = if useReflection 
                        then fr
                        else 0

    refractionAmount  = if useRefraction
                        then ft
                        else 0

    (fr,ft) = fresnel cos_i n1 (material object)

    -- common ------------------------------------------------------------------------------------
    incidentDir = (\(Ray _ d) -> d) ray
    surfNormal  = getNormalAt (geometry object) p
    surfNormal' = if isInside then (vnegate surfNormal) else surfNormal
    (n1,n2)     = (if isInside then swap else id) (1, matIOR $ material object)

    (reflectionvector, refractionvector) =
        getReflectionRefractionVectors incidentDir surfNormal' n1 n2

    cos_i       = -(incidentDir `vdot` surfNormal')  -- they should be in opposite directions, 


