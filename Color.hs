module Color where

import Utils


data Color = Color Float Float Float deriving (Show)

colorMultiply :: Color -> Color -> Color
colorMultiply (Color 0 0 0) _ = Color 0 0 0 -- improves performance?
colorMultiply (Color r1 g1 b1) (Color r2 g2 b2) = Color (multc r1 r2) (multc g1 g2) (multc b1 b2)
  where
    --multc x y = limits 0 1 $ x * y
    multc = (*)


colorMultiplyScalar :: Color -> Float -> Color
colorMultiplyScalar _ 0 = Color 0 0 0     -- improves performance?
colorMultiplyScalar (Color r g b) s = Color (multc r) (multc g) (multc b)
  where 
    --multc c = limits 0 1 c*s
    multc = (*s)

colorAdd :: Color -> Color -> Color
-- colorAdd _ (Color 1 1 1) = Color 1 1 1
colorAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (addc r1 r2) (addc g1 g2) (addc b1 b2)
  where
    --addc x y = limits 0 1 $ x + y
    addc = (+)

colorNormalize :: Color -> Color
colorNormalize (Color r g b) = Color (r/m) (g/m) (b/m)
  where
    m = max r $ max g b

colorBlend    :: Color -> Color -> Float -> Color
colorBlend source destination alpha   = (colorMultiplyScalar source alpha) 
                                       `colorAdd` 
                                        (colorMultiplyScalar destination (1-alpha))

colorClamp :: Color -> Color
colorClamp (Color r g b) = Color (m r) (m g) (m b)
  where
    m = limits 0 1