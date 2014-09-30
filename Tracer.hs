module Tracer (module Tracer) where

import Ray
import Utils
import Vector
import Geometry
import Debug.Trace
import qualified Physics

traceRay :: World -> Ray -> (Float,Float) -> Int -> Color
traceRay world ray trange maxdepth
  | maxdepth <= 0           = black
  | isNothing intersection' = bgcolor
  | otherwise               = colorDiffuse `colorMultiply` (colorShadow `colorAdd` colorMix)

  where

    useShadows = True
    useReflection = True
    useRefraction = True

    black   = Color 0 0 0
    white   = Color 1 1 1
    bgcolor = backgroundColor world

    intersection' = pickObjectAllowBackside (objects world) ray trange
    intersection = (\(Just x) -> x) intersection'   --cast down from Maybe a
    p = getp intersection
    object = fst intersection
    isInside = isBackside intersection

    -- Diffuse component
    colorDiffuse = (color object)

    -- Cast shadow rays ------------------------------------------------------------------------
    colorShadow  = if useShadows then colorShadow' else black
    colorShadow' = colorMultiplyScalar (foldl1 colorAdd $ map colorOne (lights world)) 1
      where 
        colorOne :: Light -> Color
        colorOne (Light lightPos _ _) | isNothing intersection = white
                                      | otherwise              = black
          where
            lightDir = (lightPos .-. p)
            shadowRay = spawnRay lightDir
            distToLight = vmagnitude lightDir
            intersection = pickObjectAllowBackside (objects world) shadowRay (0,distToLight)

    -- Cast reflection ray ------------------------------------------------------------------------
    colorReflection   = if useReflection 
                        then traceRay world (spawnRay reflectionvector) trange (maxdepth -1)
                        else black

    
    -- Cast refraction ray ------------------------------------------------------------------------
    colorRefraction   = if useRefraction 
                        then traceRay world (spawnRay refractionvector) trange (maxdepth -1)
                        else black

    -- Mix refraction and reflection -------------------------------------------------------------
    colorMix = (colorMultiplyScalar colorReflection (1-mix)) `colorAdd` (colorMultiplyScalar colorRefraction mix)
    -- (TODO: use fresnel equations)
    mix = case (useReflection,useRefraction) of (True,False)  -> 0
                                                (False,False) -> 1
                                                _ -> 0.5


    -- common ------------------------------------------------------------------------------------
    surfNormal = getNormalAt (geometry object) p -- what if it's inside ??
    (reflectionvector, refractionvector) = Physics.getReflectionRefractionVectors 
                                          incidentDir surfNormal 1.0 1.33
      where
        incidentDir = (\(Ray _ d) -> d) ray
        surfNormal = getNormalAt (geometry object) p -- what if it's inside ??
        (n1,n2) = (1.0, 1.33)
        (reflectionvector, refractionvector) = case isInside of
            False -> Physics.getReflectionRefractionVectors incidentDir surfNormal n1 n2
            True  -> Physics.getReflectionRefractionVectors incidentDir (vnegate surfNormal) n2 n1

    spawnRay dir = makeRay p0 dir 
      where p0 =  p .+. (dir .* 0.01) --some tolerance to avoid  numerical problems
                                      -- maybe use surfNormal * 0.01 ? What if it's inside?


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

colorAdd :: Color -> Color -> Color
--colorAdd (Color 1 1 1) _ = Color 1 1 1
--colorAdd _ (Color 1 1 1) = Color 1 1 1
colorAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (addc r1 r2) (addc g1 g2) (addc b1 b2)
  where
    --addc x y = limits 0 1 $ x + y
    addc = (+)

colorNormalize :: Color -> Color
colorNormalize (Color r g b) = Color (r/m) (g/m) (b/m)
  where
    m = max r $ max g b