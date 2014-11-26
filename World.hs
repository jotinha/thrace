module World where

import Object
import Light
import Color
import Material

data World = World { 
  objects :: [Object],
  lights  :: [Light],
  backgroundColor :: Color
}

lights2objects :: [Light] -> [Object]
lights2objects [] = []
lights2objects (x:xs) = case x of 
  Light (GeomLight geo) c -> Object geo (Emissive c) "lightObject" : rest
  _ -> rest
  where rest = lights2objects xs

objects' :: World -> [Object]
objects' world = objects world ++ lights2objects (lights world)
