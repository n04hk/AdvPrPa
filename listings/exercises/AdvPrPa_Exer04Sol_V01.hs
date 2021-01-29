module Exer04Sol where

-- Exercise (Higher-Order Functions)

import Prelude hiding (flip, curry, uncurry)

toBeImplemented = undefined

-- flip f takes its (first) two arguments in the reverse order of f
exaFlip = flip take [1,2,3,4,5] 3 == [1,2,3]

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

-- curry converts a function on pairs to a curried function
exaCurry = curry (\(x,y) -> x + y) 3 4 == 7

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

-- uncurry converts a curried function to a function on pairs
exaUncurry = uncurry (\x y -> x + y) (3, 4) == 7

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y

exaReverseFr = reverseFr [1 .. 20000] == [20000, 19999 .. 1]
exaReverseFl = reverseFl [1 .. 20000] == [20000, 19999 .. 1]

-- implement reverse using foldr and (++)
reverseFr :: [a] -> [a]
reverseFr = foldr (\x accu -> accu ++ [x]) []

-- implement reverse using foldl, (:), and flip
reverseFl :: [a] -> [a]
reverseFl = foldl (flip (:)) []

-- revAppend prepends the first list in reverse order before the second list
exaRevAppend = revAppend [3,2,1] [4,5,6] == [1,2,3,4,5,6]
-- implement revAppend using foldl and flip

revAppend :: [a] -> [a] -> [a]
revAppend = flip (foldl (flip (:)))

exaMapFr = mapFr (*2) [1 .. 10] == map (*2) [1 .. 10]
exaMapFl = mapFl (*2) [1 .. 10] == map (*2) [1 .. 10]

mapFr :: (a -> b) -> [a] -> [b]
mapFr f = foldr (\x accu -> f x : accu) []

mapFl :: (a -> b) -> [a] -> [b]
mapFl f = foldl (\accu x -> accu ++ [f x]) []

-- for estimating performance
lr = length (mapFr (*2) [1 .. 20000])
ll = length (mapFl (*2) [1 .. 20000])

exaFilterFr = filterFr odd [1 .. 10] == filterFr odd [1 .. 10]
exaFilterFl = filterFl odd [1 .. 10] == filterFl odd [1 .. 10]

filterFr :: (a -> Bool) -> [a] -> [a]
filterFr p = foldr (\x accu -> if p x then x : accu else accu) []

filterFl :: (a -> Bool) -> [a] -> [a]
filterFl p = foldl (\accu x -> if p x then accu ++ [x] else accu) []

exaLengthFr = lengthFr [1 .. 1000] == 1000
exaLengthFl = lengthFl [1 .. 1000] == 1000

lengthFr :: [a] -> Int
lengthFr = foldr (\_ accu -> 1 + accu) 0

lengthFl :: [a] -> Int
lengthFl = foldl (\accu _ -> accu + 1) 0

exaAppendFr = appendFr [1,2,3] [4,5,6] == [1 .. 6]
exaAppendFl = appendFl [1,2,3] [4,5,6] == [1 .. 6]

appendFr :: [a] -> [a] -> [a]
appendFr = flip (foldr (:))

-- strange version, since it uses the library version (++) of exactly
-- what is to be implemented
appendFl :: [a] -> [a] -> [a]
appendFl = foldl (\accu x -> accu ++ [x])
