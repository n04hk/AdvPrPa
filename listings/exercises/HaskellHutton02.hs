module Hutton.HaskellHutton02 where

dimdi = 19

double x = x + x

quadruple x = double (double x)

factorial n = product [1 .. n]

averageV1 ns = sum ns `div` length ns
averageV2 ns = div (sum ns) (length ns)

a1 = b + c
     where
       b = 1
       c = 2
d1 = a1 * 2

a2 = b + c
     where
       { b = 1 ; c = 2 }
d2 = a2 * 2

fact :: Integer -> Integer
fact 0 = 1                  -- Gleichung A
fact n = n * fact (n - 1)   -- Gleichung B

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

lastV1 xs = head (reverse xs)
lastV2 xs = head (drop (length xs - 1) xs)
lastV3 xs = xs !! (length xs - 1)

initV1 xs = take (length xs - 1) xs
initV2 xs = reverse (tail (reverse xs))

halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2
