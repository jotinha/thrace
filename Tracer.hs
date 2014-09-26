module Tracer (module Tracer) where

import Ray
import Utils
import Vector
import Debug.Trace
import qualified Physics


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

traceRay' _ _ _ EyeRay 0 = Color 0 0 0
traceRay' world ray trange EyeRay maxdepth =
  case pickObject (objects world) ray trange of
    Nothing                       -> backgroundColor world
    Just (object, Frontside p _)  -> 
      (color object) `colorMultiply`  (colorLights `colorAdd` colorReflection `colorAdd` colorRefraction)

      where
        useShadows = True
        useReflection = False && (reflection object) > 0
        useRefraction = True
        mix' = 0.5
      
        -- Cast shadow rays ------------------------------------------------------------------------
        -- TODO: implement light colors
        colorLights         = colorMultiplyScalar colorLights' 1
        colorLights'        = if useShadows
                              then foldl1 colorAdd $ map colorOneLight (lights world)
                              else (Color 1 1 1)

        -- TODO: implement color intensity, think about it, don't just multiply scalar, that only fades the shadow...
        --colorOneLight       = colorMultiplyScalar (castShadowRay light) 1
        colorOneLight (Light lightPos _ _) = traceRay' world (spawnRay lightDir) (0,distToLight) ShadowRay (maxdepth -1)
          where
            lightDir = (lightPos .-. p)
            distToLight = vmagnitude lightDir


        -- Cast reflection ray ------------------------------------------------------------------------
        colorReflection   = colorMultiplyScalar colorReflection' mix 
        colorReflection'  = if useReflection 
                            then traceRay' world (spawnRay reflectionvector) trange ReflectionRay (maxdepth -1)
                            else (Color 0 0 0)

        

        -- Cast refraction ray ------------------------------------------------------------------------
        colorRefraction   = colorMultiplyScalar colorRefraction' (1-mix)
        colorRefraction'  = if useRefraction 
                            then traceRay' world (spawnRay refractionvector) trange RefractionRay (maxdepth -1)
                            else Color 0 0 0


        -- common ------------------------------------------------------------------------------------
        incidentDir = (\(Ray _ d) -> d) ray
        surfNormal = getNormalAt (geometry object) p -- what if it's inside ??
        (reflectionvector, refractionvector) = Physics.getReflectionRefractionVectors 
                                              incidentDir surfNormal 1.0 1.33

        spawnRay dir = makeRay p0 dir 
          where p0 =  p .+. (dir .* 0.01) --some tolerance to avoid  numerical problems
                                          -- maybe use surfNormal * 0.01 ? What if it's inside?

        -- reflection - refraction mix (TODO: use fresnel equations)
        mix = case (useReflection,useRefraction) of (True,False)  -> 0
                                                    (False,False) -> 1
                                                    _ -> mix'
        


-- acts as a new eyeray for now
--TODO implement so it doesn't reflect the backgroundColor
traceRay' world ray trange ReflectionRay maxdepth = traceRay' world ray trange EyeRay maxdepth   

-- TODO: should we not spawn an internal reflection ray?
-- TODO: implement this! Right now a refraction ray does nothing when it hits the backsurface,
-- since pickObject fails
traceRay' world ray trange RefractionRay maxdepth = traceRay' world ray trange EyeRay maxdepth

traceRay' world ray trange ShadowRay _ =
  case pickObjectAllowBackside (objects world) ray trange of 
    Nothing -> Color 1 1 1 -- if shadow ray does not intersect, return identity
    _       -> Color 0 0 0 -- if shadow ray intersects either front or back, return null (shadow)


colorAdd :: Color -> Color -> Color
--colorAdd (Color 1 1 1) _ = Color 1 1 1
--colorAdd _ (Color 1 1 1) = Color 1 1 1
colorAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (addc r1 r2) (addc g1 g2) (addc b1 b2)
  where
    --addc x y = limits 0 1 $ x + y
    addc = (+)

--------------------------------------------------------
-- ALTERNATIVE --
--------------------------------------------------------

--traceRay'' :: World -> Ray -> (Float,Float) -> Float -> Color
--traceRay'' world firstray trange' maxdepth' = traceRayHere world firstray trange' EyeRay maxdepth'
  
--  where

--    traceRayHere :: Ray -> (Float,Float) -> TypeRay -> Float -> Color
    
--    traceRayHere ray trange typeray maxdepth | maxdepth <= 0 = black
--                                             | isNothing intersection = backgroundColor world
--                                             | otherwise 



--    black = Color 0 0 0
--    white = Color 1 1 1

--    makeRefractionRay :: Vector3 -> Vector3 -> Vector3 -> Float -> Vector3
--    makeRefractionRay p n d r = (r *. d) .+. (n .* (r*c - sqrt ( 1 - (r*r*(1-c*c)))))
--      where c = - (d `vdot` n)
            
--    makeReflectionRay
