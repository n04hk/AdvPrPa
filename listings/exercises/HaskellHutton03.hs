module Hutton.HaskellHutton03 where

type Vector = (Int,Int)

addvec :: (Vector,Vector) -> Vector
addvec ((x1, y1), (x2, y2)) = (x1 + x2, y1 + y2)

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> (Int -> Int)
add' x y = x + y

add5 :: Int -> Int
add5 y = add (5, y)

add'5 :: Int -> Int
add'5 = add' 5

dave = take 5 -- Dave Brubeck

multV0 x y z = x * y * z
multV1 x y = \z -> x * y * z
multV2 x = \y -> \z -> x * y * z
--multV3 :: Num a => a -> a -> a -> a
multV3 = \x -> \y -> \z -> x * y * z

second xs = head (tail xs)

swap (x,y) = (y,x)

pair x y = (x,y)

double x = x * 2

palindrome xs = reverse xs == xs

twice f x = f (f x)

neg :: (Int, Int) -> (Int, Int)
neg (x, y) = (-x, -y)
