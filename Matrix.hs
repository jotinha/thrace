module Matrix where

import Vector

data Matrix3 = Matrix3 Vector3 Vector3 Vector3 -- each vector is a line of the matrix


--[Real Time Rendering 2nd Ed, pp 71]
rotationAroundAxisMatrix3 r@(Vector3 x y z) cosa = Matrix3 
  (Vector3 (cosa + (1-cosa)*x*x)    ((1-cosa)*x*y - z*sina) ((1-cosa)*x*y + y*sina) )
  (Vector3 ((1-cosa)*x*y + z*sina)  (cosa + (1-cosa)*y*y) ((1-cosa)*y*z - x*sina) )
  (Vector3 ((1-cosa)*x*z - y*sina)  ((1-cosa)*y*z +x*sina)  (cosa + (1-cosa)*z*z)   )
  where sina = sqrt $ 1 - cosa*cosa


matMultVector3 :: Matrix3 -> Vector3 -> Vector3
matMultVector3 (Matrix3 r1 r2 r3) v = Vector3 (r1 `vdot` v) (r2 `vdot` v) (r3 `vdot` v)
  


