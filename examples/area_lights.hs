myWorld = World {
  objects = [
    -- (Object 
    --  (Inverted (makeAABoxFromPoints [Vector3 (-5) (-5) (-10), Vector3 5 5 10]))
    --  (Lambertian (Color 0.8 0.8 0.8))
    --  "room"
    --),
 
    (Object (Sphere (Vector3 (0) 0 (-4.5)) 2) 
      (BlinnPhong (Color 0.5 0.5 0.5) black 0 (Dielectric 5)) "sphere1"),
    (Object (Sphere (Vector3 (-2) 0 (-3)) 1) 
      (Lambertian (Color 1 0 0) ) "sphere2"),
    (Object (Sphere (Vector3 (4) 0 (-8)) 3) 
      (Lambertian (Color 1 0 0) ) "sphere3")
  ],
  lights = [
    --Light (PointLight (Vector3 (0) 4 (-2))) (Color 20 20 20),
    Light (GeomLight (Sphere (Vector3 3 3 (0)) 1)) (Color 50 50 50)
  ],
  backgroundColor = black
}
