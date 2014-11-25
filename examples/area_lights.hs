myWorld = World {
  objects = [
     (Object 
      (Inverted (makeAABoxFromPoints [Vector3 (-5) (-5) (-10), Vector3 5 5 10]))
      (Color 0.8 0.8 0.8)
      0 0 "room"
    ),
 
    (Object (Sphere (Vector3 (0) 0 (-4.5)) 2) (Color 0.5 0.5 0.5) 1 1 "sphere1"),
    (Object (Sphere (Vector3 (-2) 0 (-3)) 1) (Color 1 0 0) 0 0 "sphere2"),
    (Object (Sphere (Vector3 (4) 0 (-8.1)) 3) (Color 1 0 0) 0 0 "sphere3")
  ],
  lights = [
    --Light (PointLight (Vector3 (0) 4 (-2))) (Color 20 20 20),
    Light (GeomLight (Sphere (Vector3 3 3 (0)) 0.5)) (Color 30 30 30)
  ],
  backgroundColor = Color 0 0 0
}