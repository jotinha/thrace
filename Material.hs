module Material where

import Color

data Material 
  = Lambertian { diffuseColor :: Color }
  | BlinnPhong { diffuseColor :: Color, specularColor :: Color, shininess :: Float, medium :: MaterialMedium}
  | Emissive   { emissionColor :: Color } 

data MaterialMedium
  = Dielectric { ior :: Float} 
  | Conductor { ior :: Float, absorptionCoeff :: Float }


matIOR :: Material -> Float
matIOR BlinnPhong{medium=medium} = ior medium
matIOR _ = error "No ior for this material"

getDiffuseComponent :: Material -> Color
getDiffuseComponent (Lambertian c) = c
getDiffuseComponent (Emissive _)   = black
getDiffuseComponent BlinnPhong{diffuseColor = c} = c

getSpecularComponent :: Material -> Float -> Color
getSpecularComponent (Lambertian _) _ = black
getSpecularComponent (Emissive _) _ = black
-- cos_h is cos of angle between view and half vector
-- for physically plausible results, cspec should be fresnel(cos_h) 
getSpecularComponent BlinnPhong{specularColor = cspec, shininess=m} cos_h = 
  cspec `colorMultiplyScalar` (((m + 8) / 8) * (cos_h ** m))


