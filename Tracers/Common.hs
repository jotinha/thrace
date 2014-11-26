module Tracers.Common (module Tracers.Common) where

import Material
import Physics
import Vector
import World
import Color
import Light
import Random
import Data.List (elemIndex)
import Utils
import Ray
import Object

useShading = True
useShadows = True
useReflection = True
useRefraction = True
allowTransparentShadows = True -- Only for tracers that support transparency

fresnel :: Float -> Float -> Material -> (Float,Float)
fresnel _ _ (Lambertian _) = (0,0) --no reflection or refraction
fresnel cos_i ni mat = case (medium mat) of Dielectric  n   -> fresnelDiel cos_i ni n
                                            Conductor   n k -> fresnelCond cos_i n k
-- localIllumination : shades surface at point p given lights in scene
-- world        : world
-- v            : view vector (normalized), from p to eye
-- p            : surface point
-- n            : surface normal, pointing out, not necessarily in v direction
-- matBRDF      : BRDF function
-- nsr          : number of shadow rays to cast
-- useTransparentShadows : whether to use semi-transparent shadows.
localIllumination :: World -> Vector3 -> Vector3 -> Vector3 -> BRDF -> Int -> Bool -> Color
localIllumination world v p n matBRDF nsr useTransparentShadows = 
  colorSum $ map colorOneLight (lights world)
  where
    colorOneLight light@((Light lightType lightColor)) = 
      colorAverage $ map sampleOnce (raysToLight lightType p (Just (nsr, seed)))
      where
        seed = generateSeed [idx] [p,v,lightPos lightType]
        (Just idx) = elemIndex light (lights world)
        
        sampleOnce :: Vector3 -> Color
        sampleOnce vecToLight 
          | nsr == 0 || not useShadows  = lightComponent
          | shadowAlpha  == 0           = black
          | otherwise                   = lightComponent `colorMultiplyScalar` shadowAlpha
          where
            lightComponent | ccos_light == 0 = black
                           | otherwise = (matBRDF l v n) `colorMultiply` lightIrradianceHere
      
            -- should l,v and n be in the same side of the surface ?? 
            d = vmagnitude vecToLight
            l = vecToLight ./ d 

            --l' is relative to center of light. This is *wrong* but avoids noise for surfaces
            --illuminated by area lights.
            --l' = vnormalize $ (lightPos lightType) .-. p
            ccos_light = max 0 $ l `vdot` n --what if it's inside object??
            lightIrradianceHere = lightColor `colorMultiplyScalar` (ccos_light * (lightFalloff lightType d))

            shadowRay = spawnRay p l
            --don't include emissive sources in objects
            shadowAlpha = case pickObjectAllowBackside (objects world) shadowRay (0,d) of
              Nothing      -> 1
              Just (obj,_) -> if useTransparentShadows then getShadowAlpha $ material obj else 0


spawnRay :: Vector3 -> Vector3 -> Ray
spawnRay p dir = Ray p0 dir'
  where 
    dir' = vnormalize dir
    p0 =  p .+. (dir' .* 0.01) --some tolerance to avoid  numerical problems
                                  -- maybe use surfNormal * 0.01 ? What if it's inside?
