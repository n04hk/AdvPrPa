module Hutton.HaskellHutton06 where

import Prelude hiding ((++), and, concat, replicate, (!!), elem)

infixr 5 ++

myDrop n xs
  | n <= 0 = xs
myDrop _ [] = []
myDrop n (x : xs) = drop (n-1) xs

(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

reversePre :: [a] -> [a]
reversePre []       = []
reversePre (x : xs) = reversePre xs ++ [x]

reverseAccu :: [a] -> [a]
reverseAccu xs = h xs []
  where
    h []       accu = accu
    h (x : xs) accu = h xs (x : accu)

-- as specified in the Prelude
reverseFold :: [a] -> [a]
reverseFold = foldl (flip (:)) []

and :: [Bool] -> Bool
and []       = True
and (x : xs) = x && and xs

concat :: [[a]] -> [a]
concat []         = []
concat (xs : xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
_        !! n | n < 0 = error "negative index"
[]       !! _         = error "index too large"
(x : _)  !! 0 = x
(x : xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ []                   = False
elem x (y : ys) | y == x    = True
                | otherwise = elem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
--msort [x] = [x]
msort xs  = merge (msort (take n xs)) (msort (drop n xs))
  where n = length xs `div` 2

halve :: [a] -> ([a], [a])
halve xs = h xs [] []
  where
    h (x1 : x2 : xs) accu1 accu2 = h xs (x1 : accu1) (x2 : accu2)
    h [x]            accu1 accu2 = (x : accu1, accu2)
    h []             accu1 accu2 = (accu1, accu2)

msortV2 []  = []
msortV2 [x] = [x]
msortV2 xs = merge (msortV2 hsl) (msortV2 hsr)
  where (hsl, hsr) = halve xs
