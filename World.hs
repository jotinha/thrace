module World where

import Object
import Light
import Color

data World = World { 
  objects :: [Object],
  lights  :: [Light],
  backgroundColor :: Color
}
