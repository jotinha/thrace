module Tracer (module Tracer) where

import Ray
import Utils
import Vector

colorMultiply :: Color -> Float -> Color
colorMultiply (Color r g b) s = Color (multc r) (multc g) (multc b)
  where 
    multc c = limits 0 1 c*s

traceRay :: World -> Ray -> (Float,Float) -> Color
traceRay world ray trange = 
  case pickObject (objects world) ray trange of
    Nothing -> backgroundColor world
    Just (object,(Frontside p _)) -> colorMultiply (color object) lightsItensity
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


