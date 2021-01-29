module Hutton.HaskellHutton05 where

pairs xs = zip xs (tail xs)

sorted xs = and [x <= y | (x, y) <- pairs xs]

s1 = sorted ([] :: [Int])

isPyth :: (Integer, Integer, Integer) -> Bool
isPyth (x,y,z) = x^2 + y^2 == z^2

pythsV1 :: Integer -> [(Integer, Integer, Integer)]
pythsV1 n =
  [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], isPyth (x,y,z)]

-- finds all triples with x <= y
pythsV2 :: Integer -> [(Integer, Integer, Integer)]
pythsV2 n =
  [(x,y,z) | z <- [1..n],
             y <- [1..z-1],
             x <- [1..y], isPyth (x,y,z)]
    where
      isPyth (x,y,z) = x^2 + y^2 == z^2

allWithXeqY :: [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
allWithXeqY xs = [(x, y, z) | (x, y, z) <- xs, x == y]

allWithXltY :: [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
allWithXltY xs = [t | t@(x, y, _) <- xs, x < y]

factorsWOn :: Integer -> [Integer]
factorsWOn n = [x | x <- [1..n-1], n `mod` x == 0]

perfectsV1 :: Integer -> [Integer]
perfectsV1 n = [x | x <- [1..n], x == sum (factorsWOn x)]

perfectsV2 :: Integer -> [Integer]
perfectsV2 n = [x | x <- [1..n], x == sum (factorsWOn x)]
  where
    factorsWOn n = [x | x <- [1..n-1], n `mod` x == 0]

scalProd :: [Int] -> [Int] -> Int
scalProd xs ys = sum [x * y | (x,y) <- zip xs ys]

positions start x xs =
  [i | (x', i) <- zip xs [start ..], x' == x]
