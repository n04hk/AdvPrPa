module Exer01Sol where

-- Develop some functions to work with two-dimensional vectors.

-- a type for two-dimensional vectors
type Vec = (Double, Double)

-- the zero vector
zeroVec :: Vec
zeroVec = (0, 0)

-- some example vectors
a, b, c, d :: Vec
a = (3, 0)
b = (0, 4)
c = (sqrt2, sqrt2)
      where sqrt2 = sqrt 2
d = (3, 4)

-- lengthVec computes the length of a vector.
exaLengthVec =
  lengthVec a == 3 && lengthVec c == 2

lengthVec :: Vec -> Double
lengthVec (x, y) = sqrt (x^2 + y^2)

-- negVec negates a vector.
exaNegVec =
  negVec d == (-3, -4)

negVec :: Vec -> Vec
negVec (x, y) = (-x, -y)

-- negVecCurry negates a vector, but uses currying.
-- Note: this is a bad use of currying, since
-- the two components of a vector belong together.
exaNegVecCurry =
  negVecCurry (fst d) (snd d) == (-3, -4)

negVecCurry :: Double -> Double -> Vec
negVecCurry x y = (-x, -y)

-- addVec adds two vectors.
exaAddVec =
  a `addVec` b == d

addVec :: Vec -> Vec -> Vec
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- subVec subtracts two vectors.
exaSubVec =
  a `subVec` b == (3, -4)
-- Implement this function using negVec and addVec.

subVec :: Vec -> Vec -> Vec
subVec v1 v2 = addVec v1 (negVec v2)

subVecTerrible :: Vec -> Vec -> Vec
subVecTerrible (v1x, v1y) (v2x, v2y) = addVec (v1x, v1y) (negVec (v2x, v2y))
-- This is terrible, since there is no need to resolve
-- the components of the vectors.

-- subVecCurry subtracts two vectors.
-- Note: this example clearly demonstrates the bad use
-- of currying in negVecCurry.
-- The two components of a vector simply belong together.
exaSubVecCurry =
  a `subVecCurry` b == (3, -4)
-- Implement this function using negVecCurry and addVec.

subVecCurryV1 :: Vec -> Vec -> Vec
subVecCurryV1 v1 v2 = addVec v1 (negVecCurry (fst v2) (snd v2))
-- Whenever we see usage of fst and/or snd, we should automatically
-- strive to use pattern matching instead.
-- This is usually more elegant; see the next version.

subVecCurryV2 :: Vec -> Vec -> Vec
subVecCurryV2 v1 (v2x, v2y) = addVec v1 (negVecCurry v2x v2y)

subVecCurry = subVecCurryV2

-- distance computes the distance between two vectors.
exaDistance =
  distance a d == 4
-- Implement this function using subVec and lengthVec.

distance :: Vec -> Vec -> Double
distance v1 v2 = lengthVec (v1 `subVec` v2)

-- Scales a vector with a factor.
exaScaleVec =
  scaleVec d 3 == (9, 12)

scaleVec :: Vec -> Double -> Vec
scaleVec (x, y) s = (s*x, s*y)
