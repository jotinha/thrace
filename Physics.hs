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

-- for dielectric only
fresnel :: Float -> Float -> Float -> (Float,Float)
fresnel cos_i n1 n2 | tir = (1,0)
                    | otherwise = fresnel' cos_i cos_t n1 n2
  where
    tir = sin_t2 > 1
    cos_t  = sqrt(1 - sin_t2)
    sin_t2 = ((n1/n2)^2)*sin_i2
    sin_i2  = 1 - cos_i^2

fresnel' :: Float -> Float -> Float -> Float -> (Float,Float)
fresnel' cos_i cos_t n1 n2 | cos_i < 0 - tol || cos_i > 1 + tol = error $ "bad cos_i " ++ (show cos_i)
                           | cos_t < 0 - tol || cos_t > 1 + tol = error $ "bad cos_t " ++ (show cos_t)
                           | otherwise = (r,t)
  where
    r_normal    = ((n1*cos_i - n2*cos_t) / (n1*cos_i + n2*cos_t))^2
    r_parallel  = ((n2*cos_i - n1*cos_t) / (n2*cos_i + n1*cos_t))^2
    r           = 0.5 * (r_normal + r_parallel)
    t           = 1 - r
    tol = 1e-5

fresnelSchlickApprox :: Float -> Float -> Float -> (Float,Float)
fresnelSchlickApprox cos_i n1 n2  | n1 <= n2 = shlick cos_i
                                  | n1 > n2 && (not tir) = shlick cos_t
                                  | otherwise = (1,0)

  where

    shlick cosx = let fr = r0 + (1-r0)*((1-cosx)^5) in (fr,1-fr)

    r0 = ((n1 - n2) / (n1 + n2))^2
    sin_t2 = ((n1/n2)^2)*(1-cos_i^2)
    tir = sin_t2 > 1    
    cos_t = sqrt(1-sin_t2)
