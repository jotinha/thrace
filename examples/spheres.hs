myWorld = World {
  objects = [
    (Object 
      (Inverted (makeAABoxFromPoints [Vector3 (-5) (-5) (-10), Vector3 5 5 10]))
      (Color 0.8 0.8 0.8)
      0 0 "room"
    ),
    (Object (Sphere (Vector3 (0) 0 (-5)) 2) (Color 1 1 1) 1 0 "sphere1"),
    (Object (Sphere (Vector3 (-2) 0 (-3)) 1) (Color 1 0 0) 0 0 "sphere2"),
    (Object (Sphere (Vector3 (4) 0 (-8)) 3) (Color 1 0 0) 0 0 "sphere3")
  ],
  lights = [
    Light (PointLight (Vector3 (0) 4 (-2))) (Color 20 20 20),
    Light (PointLight (Vector3 (0) (-3) (2))) (Color 10 10 10)
  ],
  backgroundColor = Color 1 1 1
}