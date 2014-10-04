module Tracer (module Tracer) where

import Ray
import Utils
import Vector
import Geometry
import Debug.Trace
import qualified Physics
import Data.Tuple (swap)

traceRay :: World -> Ray -> (Float,Float) -> Int -> Color
traceRay world ray trange maxdepth
  | maxdepth <= 0           = white
  | isNothing intersection' = bgcolor
  | isDiffuse               = colorObject `colorMultiply` colorLights
  | otherwise               = (colorObject `colorMultiply` colorMix)

  where

    useShading = True
    useShadows = False
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
    isDiffuse = (transparency object) == 0 && (reflection object) == 0

    colorObject = (color object)
    
    -- Cast shadow rays ------------------------------------------------------------------------
    colorLights  = foldl colorAdd black $ map colorOneLight (lights world)
      where 
        colorOneLight :: Light -> Color
        colorOneLight (Light lightPos _ _)  | not useShading = colorObject
                                            | not useShadows = lightComponent
                                            | isNothing shadowIntersection = lightComponent
                                            | otherwise = black
          where
            lightComponent = colorMultiplyScalar white (ccos_light)                                  
            ccos_light = max 0 $ (vnormalize lightDir) `vdot` surfNormal
            lightDir = (lightPos .-. p)
            shadowRay = spawnRay lightDir
            distToLight = vmagnitude lightDir

            shadowIntersection = pickObjectAllowBackside (objects world) shadowRay (0,distToLight)

    -- Cast reflection ray ------------------------------------------------------------------------
    colorReflection   = traceRay world (spawnRay reflectionvector) trange (maxdepth -1)
                        
    -- Cast refraction ray ------------------------------------------------------------------------
    colorRefraction   = traceRay world (spawnRay refractionvector) trange (maxdepth -1)

    -- Mix refraction and reflection -------------------------------------------------------------
    colorMix = if reflectionAmount > 0 || refractionAmount > 0
               then (colorMultiplyScalar colorReflection $ reflectionAmount) 
                      `colorAdd` 
                    (colorMultiplyScalar colorRefraction $ refractionAmount) 
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
-- colorAdd _ (Color 1 1 1) = Color 1 1 1
colorAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (addc r1 r2) (addc g1 g2) (addc b1 b2)
  where
    --addc x y = limits 0 1 $ x + y
    addc = (+)

colorNormalize :: Color -> Color
colorNormalize (Color r g b) = Color (r/m) (g/m) (b/m)
  where
    m = max r $ max g b

colorBlend    :: Color -> Color -> Float -> Color
colorBlend source destination alpha   = (colorMultiplyScalar source alpha) 
                                       `colorAdd` 
                                        (colorMultiplyScalar destination (1-alpha))

