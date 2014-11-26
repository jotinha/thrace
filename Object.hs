module Object where

import Geometry
import Color
import Material

data Object = Object {
  geometry      :: Geometry,
  material      :: Material,
  objId         :: String
}


-- --lights as geometry
--pointLight p c = Object (Sphere p 0) (Emissive c)  "pointLight"
--geomLight  g c = Object g (Emissive c) "geoLight"

--isPointLight :: Object -> Bool
--isPointLight (Object (Sphere _ 0) (Emissive _)  _) = True
--isPointLight _ = False

----check if it is a light which is not point Light
--isGeomLight :: Object -> Bool
--isGeomLight o = (isLight o) && (not $ isPointLight o)

--isLight :: Object -> Bool
--isLight (Object _ (Emissive _) _) = True
--isLight _ = False