module Physics (module Physics) where

import Vector

-- expects normalized input vectors
getReflectionRefractionVectors :: Vector3 -> Vector3 -> Float -> Float -> (Vector3,Vector3)
getReflectionRefractionVectors incident normal n1 n2 = (reflection, refraction)
  where
    d = incident
    n = normal
    reflection = d .-. ((2 * (n `vdot` d)) *. n)  --reflection direction. n must be normalized 

    r = n1/n2
    c = - (d `vdot` n)
    refraction = (r *. d) .+. (n .* (r*c - sqrt ( 1 - (r*r*(1-c*c)))))
    -- https://en.wikipedia.org/w/index.php?title=Snell%27s_law&oldid=621864417

