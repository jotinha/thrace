module Tracer (module Tracer) where

import Ray
import Utils
import Vector
import Geometry
import Color
import Object
import Light
import World
import qualified Physics
import Data.Tuple (swap)
import Data.List (elemIndex)
import Random

traceRay :: World -> Ray -> (Float,Float) -> Int -> Color
traceRay world ray trange maxdepth
  | maxdepth <= 0           = white
  | isNothing intersection' = bgcolor
  | isDiffuse               = (colorObject `colorMultiply` colorLights) --`debug` "diffuse"
  | otherwise               = colorObject `colorMultiply` (colorLights `colorAdd` colorMix)

  where

    useShading = True
    useShadows = True
    useReflection = True
    useRefraction = True
    numberShadowRays = 1

    black   = Color 0 0 0
    white   = Color 1 1 1
    bgcolor = backgroundColor world

    intersection' = pickObjectAllowBackside (objects world) ray trange
    intersection = (\(Just x) -> x) intersection'   --cast down from Maybe a
    p = getp intersection
    object = fst intersection
    isInside = isBackside intersection
    isDiffuse = (transparency object) == 0 && (reflection object) == 0

    colorObject = (color object)

    -- Cast shadow rays ------------------------------------------------------------------------
    colorLights = colorSum $ map colorOneLight (lights world)

      where 
        colorOneLight :: Light -> Color
        colorOneLight light@((Light lightType lightColor)) = 
          colorAverage $ map sampleOnce (raysToLight lightType p (Just (numberShadowRays,seed)))
          where
            seed = generateSeed [idx] [p,incidentDir,lightPos lightType]
            (Just idx) = elemIndex light (lights world)
            
            sampleOnce :: Vector3 -> Color
            sampleOnce lightDir
              | not useShading = colorObject
              | not useShadows = lightComponent
              | isNothing shadowIntersection = lightComponent
              | otherwise = black
              where
                lightComponent = colorMultiplyScalar lightColor (ccos_light * (lightFalloff lightType distToLight))
                ccos_light = max 0 $ (vnormalize lightDir) `vdot` surfNormal

                shadowRay = spawnRay lightDir
                distToLight = vmagnitude lightDir

                shadowIntersection = pickObjectAllowBackside (objects world) shadowRay (0,distToLight)

               
    -- Cast reflection ray ------------------------------------------------------------------------
    colorReflection   = traceRay world (spawnRay reflectionvector) trange (maxdepth -1)
                        
    -- Cast refraction ray ------------------------------------------------------------------------
    colorRefraction   = traceRay world (spawnRay refractionvector) trange (maxdepth -1)

    -- Mix refraction and reflection -------------------------------------------------------------
    colorMix = if reflectionAmount > 0 || refractionAmount > 0
               then (colorMultiplyScalar colorReflection $ reflectionAmount / (reflectionAmount + refractionAmount)) 
                      `colorAdd` 
                    (colorMultiplyScalar colorRefraction $ refractionAmount / (reflectionAmount + refractionAmount)) 
               else white
    
    reflectionAmount  = if useReflection 
                        then fr
                        else 0

    refractionAmount  = if useRefraction
                        then (transparency object) * ft
                        else 0

    (fr,ft) = Physics.fresnel cos_i n1 n2
    --(fr,ft) = Physics.fresnelSchlickApprox cos_i n1 n2
    --fr = 0.1 + 0.9*((1 - cos_i)^5)
    --ft = 1 - fr

    -- common ------------------------------------------------------------------------------------
    incidentDir = (\(Ray _ d) -> d) ray
    surfNormal  = getNormalAt (geometry object) p
    surfNormal' = if isInside then (vnegate surfNormal) else surfNormal
    (n1,n2)     = (if isInside then swap else id) (1, 3)

    (reflectionvector, refractionvector) =
        Physics.getReflectionRefractionVectors incidentDir surfNormal' n1 n2

    cos_i       = -(incidentDir `vdot` surfNormal')  -- they should be in opposite directions, 

    spawnRay dir = Ray p0 dir'
      where 
        dir' = vnormalize dir
        p0 =  p .+. (dir' .* 0.01) --some tolerance to avoid  numerical problems
                                      -- maybe use surfNormal * 0.01 ? What if it's inside?
