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
import Geometry

useShading = True
useShadows = True
useReflection = True
useRefraction = True
allowTransparentShadows = True -- Only for tracers that support transparency

fresnel :: Float -> Float -> Material -> (Float,Float)
fresnel _ _ (Lambertian _) = (0,0) --no reflection or refraction
fresnel cos_i ni mat = case (medium mat) of Dielectric  n   -> fresnelDiel cos_i ni n
                                            Conductor   n k -> fresnelCond cos_i n k
-- localIllumination : shades surface at point x given lights in scene
-- world        : world
-- v            : view vector (normalized), from x to eye
-- x            : surface point
-- n            : surface normal, pointing out, not necessarily in v direction
-- matBRDF      : BRDF function
-- nsr          : number of shadow rays to cast
-- useTransparentShadows : whether to use semi-transparent shadows.
localIllumination :: World -> Vector3 -> Vector3 -> Vector3 -> BRDF -> Int -> Bool -> Color
localIllumination world v x n matBRDF nsr useTransparentShadows = colorSum $
  map colorOneLight (lights world)
  where
    colorOneLight light@((Light lightType lightColor)) = colorAverage $
      map evalLightPoint samples
      where
        samples = sampleLight lightType (x,n) nsr seed
        seed = generateSeed [idx] [x,v,lightPos lightType]
        (Just idx) = elemIndex light (lights world)
        
        evalLightPoint :: (Vector3,Float,Float) -> Color
        evalLightPoint (x',cos_theta', p_x') 
          | nsr == 0 || not useShadows  = lightComponent
          | shadowAlpha  == 0           = black
          | otherwise                   = lightComponent `colorMultiplyScalar` shadowAlpha
          where
            lightComponent | ccos_i == 0 = black
                           | otherwise = (matBRDF l v n) `colorMultiply` lightIrradianceHere
      
            -- should l,v and n be in the same side of the surface ?? 
            vecToLight = x' .-. x
            d = vmagnitude vecToLight
            l = vecToLight ./ d 

            --l' is relative to center of light. This is *wrong* but avoids noise for surfaces
            --illuminated by area lights.
            --l' = vnormalize $ (lightPos lightType) .-. p
            ccos_i = max 0 $ l `vdot` n --what if it's inside object??
            lightIrradianceHere = lightColor `colorMultiplyScalar` (
                                    ccos_i * (lightFalloff lightType d) * cos_theta' / p_x'
                                  )

            shadowRay = spawnRay x l
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

--TODO: we should calculate the area properly for point and directional...
-- Or maybe we shouldn't care about area at all and use empirical values for the colors
-- or calculate color at the surface properly
sampleLight :: LightType -> (Vector3, Vector3) -> Int -> Seed -> [(Vector3, Float, Float)]
sampleLight  _ _ 0 _                        = []
sampleLight (PointLight c) (x,_) _ _        = [(c, 1,1)]
sampleLight (DirectionalLight d) (x,_) _ _  = [(x .+. d,1,1)] --this is a point close to x, and not really infinite, but should work out
sampleLight (GeomLight geo) (x,_) 1 _       = [(centerOf geo, 1, 1/(areaOf geo))] -- when nsamples = 1 just treat geometry as point light
sampleLight (GeomLight sphere@Sphere{}) xn nsamples g = sampleSphericalLuminaireBetter sphere xn nsamples g
sampleLight (GeomLight geo) xn nsamples g = sampleGenericLuminaire geo xn nsamples g

sampleSphericalLuminaireBetter :: Geometry -> (Vector3, Vector3) -> Int -> Seed -> [(Vector3, Float, Float)]
sampleSphericalLuminaireBetter (Sphere c r) (x,n) nsamples g = take nsamples $ 
  randomRemapFromCanonical2 randPoint g
  where
    randPoint (u0,u1) = (x', cos_theta', p_x')
      where
        --max radius
        d = c .-. x
        cos_alpha_max = sqrt (1 - r*r/(vsq d))
        
        --define orhonormal basis uvw
        n = if abs (up `vdot` w) > tol then up else fwd
        tol = 1e-2

        w = vnormalize $ c .-. x
        v = vnormalize $ w `vcross` n
        u = vnormalize $ v `vcross` w

        --compute perturbed ray
        k_i = (u .* ((cos phi)*sin_alpha)) .+. 
              (v .* ((sin phi)*sin_alpha)) .+. 
              (w .* cos_alpha)

        -- intersection of ray with sphere gets the point
        x' = case rayIntersect (Ray x k_i) (Sphere c (r+tol)) of 
              Frontside p _ -> p
              otherwise     -> error "expected intersection with sphere luminaire"
            where tol = 0.01

        -- probability distributions
        q = 1 / (2*pi*(1 - cos_alpha_max))
        p_x' = cos_theta' * q / dist2
        dist2 = vsq $ x' .-. x
        cos_theta' = - ((vnormalize $ x' .-. c) `vdot` (vnormalize d))

        --random variables
        cos_alpha = 1 - u0 + u0 * cos_alpha_max
        phi       = 2*pi*u1
        sin_alpha = sqrt $ 1 - cos_alpha*cos_alpha


sampleGenericLuminaire :: Geometry -> (Vector3, Vector3) -> Int -> Seed -> [(Vector3, Float, Float)]
sampleGenericLuminaire geo (x,n) nsamples seed = take nsamples $ 
  filter (\(_,cos_t',_) -> cos_t' > 0) $ map f $ randomPointsOnSurfaceOf geo seed
  --TODO: since we are filtering, we should divide the area of the geo by the fraction in view
  where
    f x' = (x', cos_theta', 1/(areaOf geo))
      where
        n' = getNormalAt geo x'
        cos_theta' = n' `vdot` (vnormalize (x .-. x'))



