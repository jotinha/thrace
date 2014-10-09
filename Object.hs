module Object where

import Geometry
import Color

data Object = Object {
  geometry      :: Geometry,
  color         :: Color,
  reflection    :: Float,
  transparency  :: Float,
  objId         :: String
}