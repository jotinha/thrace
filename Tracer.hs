module Tracer (module Tracer) where

import Ray
import Utils
import Vector
import Debug.Trace


traceRay :: World -> Ray -> (Float,Float) -> Color
traceRay world ray trange = 
  case pickObject (objects world) ray trange of
    Nothing -> backgroundColor world
    Just (object,(Frontside p _)) -> colorMultiplyScalar (color object) lightsItensity
      where
        lightsItensity = sum [ intensity | light@(Light _ _ intensity) <- (lights world), not (inShadowOf light)]

        inShadowOf :: Light -> Bool
        inShadowOf (Light lightPos _ _) = not $ isEmpty  $ rayIntersectionsWithinRange (0,distToLight) objects' lightray
          where 
            objects' = objects world
            dir = (lightPos .-. p)
            p0 =  p .+. (dir .* 0.001) --some tolerance to avoid  numerical problems
            lightray = makeRay p0 dir 
            distToLight = vmagnitude dir


--trace :: Num n => World -> Ray -> (Float,Float) -> n -> Color
--trace world ray trange maxdepth = mixColors $ traceRayHere ray trange EyeRay maxdepth

--where
  
--  traceRayHere :: Ray -> (Float,Float) -> TypeRay -> n -> [Color]
  
--  traceRayHere _ _ _ _ 0 = []
--  traceRayHere _ _ ShadowRay maxdepth = 

--  traceRayHere _ _ EyeRay maxdepth





--trace :: Num b => World -> Ray -> (Float,Float) -> n -> Color
--trace world ray trange maxdepth = mapColors $ traceRayHere ray trange EyeRay maxdepth
--where 

  

--  data TypeRay = EyeRay | ShadowRay | ReflectionRay | RefractionRay

colorMultiply :: Color -> Color -> Color
colorMultiply (Color 0 0 0) _ = Color 0 0 0 -- improves performance?
colorMultiply (Color r1 g1 b1) (Color r2 g2 b2) = Color (multc r1 r2) (multc g1 g2) (multc b1 b2)
  where
    --multc x y = limits 0 1 $ x * y
    multc = (*)


colorMultiplyScalar :: Color -> Float -> Color
colorMultiplyScalar _ 0 = Color 0 0 0     -- improves performance?
colorMultiplyScalar (Color r g b) s = Color (multc r) (multc g) (multc b)
  where 
    --multc c = limits 0 1 c*s
    multc = (*s)


data TypeRay = EyeRay | ShadowRay | ReflectionRay | RefractionRay

traceRay' :: World -> Ray -> (Float,Float) -> TypeRay -> Float -> Color

traceRay' _ _ _ EyeRay 0 = Color 1 1 1
traceRay' world ray trange EyeRay maxdepth =
  case pickObject (objects world) ray trange of
    Nothing                       -> backgroundColor world
    Just (object, Frontside p _)  -> colorDiffuse `colorMultiply`  (colorLights `colorAdd` colorReflection `colorAdd` colorRefraction)

      where
        useShadows = True
        useReflection = True && (reflection object) > 0
        --useTransmission = False
        mix = 0.5

        colorDiffuse = color object

        colorLights = if useShadows 
                      then foldl1 colorAdd $ map colorShadowRay (lights world)
                      else (Color 1 1 1)

        colorShadowRay light@(Light _ lightColor intensity) = colorMultiplyScalar (castShadowRay light) intensity
        castShadowRay (Light lightPos _ _) = traceRay' world shadowray (0,distToLight) ShadowRay (maxdepth -1)
          where
            dir = (lightPos .-. p)
            p0 =  p .+. (dir .* 0.001) --some tolerance to avoid  numerical problems
            shadowray = makeRay p0 dir 
            distToLight = vmagnitude dir

        colorReflection =  if useReflection then colorMultiplyScalar castReflectionRay mix else (Color 0 0 0)
        castReflectionRay = traceRay' world reflectionray trange ReflectionRay (maxdepth -1)
          where
            n = getNormalAt (geometry object) p 
            d = (\(Ray _ d) -> d) ray
            dir = d .-. ((2 * (n `vdot` d)) *. n)  --reflection direction. n must be normalized 
                                                   -- n dot d should be negative (guaranteed by frontside ?)
            p0 =  p .+. (dir .* 0.001) --some tolerance to avoid  numerical problems
            reflectionray = makeRay p0 dir 

        colorRefraction = Color 0 0 0
    
-- acts as a new eyeray for now
--traceRay' world ray trange ReflectionRay maxdepth = traceRay' world ray trange EyeRay maxdepth   
traceRay' world ray trange ReflectionRay maxdepth =
  case pickObject (objects world) ray trange of 
    Nothing -> Color 1 1 1
    Just (object, Frontside p _) -> color object


traceRay' world ray trange ShadowRay _ =
  case pickObjectAllowBackside (objects world) ray trange of 
    Nothing -> Color 1 1 1 -- if shadow ray does not intersect, return identity
    _       -> Color 0 0 0 -- if shadow ray intersects either front or back, return null (shadow)

-- mix diffuse, shadow, reflection and transmission colors
--mixDSRT :: Color -> Color -> Color -> Color -> Float -> Float -> Color
--mixDSRT diffuseColor shadowColor reflectionColor transmissionColor reflectionAmount transparency = 
--  colorMultiply

--where
--  color1 = colorMultiply diffuseColor shadowColor
--  color2 = colorMultiplyScalar reflectionColor reflectionAmount
--  color3 = colorMultiplyScalar transmissionColor transparency
--  colorRT = colorAdd color2 color3
--  --facingratio = -ndotd
  --fresneleffect = mix ((1 - facingratio) ^ 3) 1 0.1
  --mix a b ratio = (b*ratio) + (a * (1 - ratio)

colorAdd :: Color -> Color -> Color
--colorAdd (Color 1 1 1) _ = Color 1 1 1
--colorAdd _ (Color 1 1 1) = Color 1 1 1
colorAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (addc r1 r2) (addc g1 g2) (addc b1 b2)
  where
    --addc x y = limits 0 1 $ x + y
    addc = (+)