module Material where

import Color
import Vector
import Utils

data Material 
  = Lambertian { diffuseColor :: Color}
  | BlinnPhong { diffuseColor :: Color, specularColor :: Color, shininess :: Float, medium :: MaterialMedium}
  | Emissive   { emissionColor :: Color } 

data MaterialMedium
  = Dielectric { ior :: Float} 
  | Conductor { ior :: Float, absorptionCoeff :: Float }

matIOR :: Material -> Float
matIOR BlinnPhong{medium=medium} = ior medium
matIOR _ = error "No ior for this material"

getDiffuseComponent :: Material -> Color
getDiffuseComponent (Lambertian c) = c --`colorMultiplyScalar` (1/pi)
getDiffuseComponent (Emissive _)   = black 
getDiffuseComponent BlinnPhong{diffuseColor = c} = c

getSpecularComponent :: Material -> Float -> Color
getSpecularComponent (Lambertian _) _ = black
getSpecularComponent (Emissive _) _ = black
-- cos_h is cos of angle between view and half vector
-- for physically plausible results, cspec should be fresnel(cos_h) 
getSpecularComponent BlinnPhong{specularColor = cspec, shininess=m} cos_h = 
  cspec `colorMultiplyScalar` (((m + 8) / 8) * (cos_h ** m))


type BRDF = Vector3 -> Vector3 -> Vector3 -> Color
-- brdf takes a material and three vectors and returns a color
-- mat : material
-- v : view vector (normalized)
-- l : light vector (normalized)
-- n : surface normal (normalized)
brdf :: Material -> Vector3 -> Vector3 -> Vector3 -> Color
brdf Lambertian{diffuseColor=c} _ _ _ = c
brdf BlinnPhong{diffuseColor=c,specularColor=s,shininess=m} v l n = 
  c `colorAdd` (s `colorMultiplyScalar` (((m + 8) / 8) * (cos_half ** m)))
  where 
    cos_half = h `vdot` n
    h = (v .+. l) ./ 2          --half vector
      

getShadowAlpha :: Material -> Float
getShadowAlpha Lambertian{} = 0   -- fully opaque, black shadow
getShadowAlpha Emissive{}   = 1   -- Does not cast shadow
getShadowAlpha mat@BlinnPhong{} = 1 - (limits 0 1 $ ((matIOR mat) - 1) / (8 - 1))
  -- fake this for now, the bigger the ior, the more reflective, the lower alpha