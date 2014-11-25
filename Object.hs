module Object where

import Geometry
import Color
import Material

data Object = Object {
  geometry      :: Geometry,
  material      :: Material,
  objId         :: String
}