module Hutton.HaskellHutton04 where

--signumV1 :: (Num a, Ord a) => a -> Int
signumV1 n | n < 0     = -1
           | n == 0    =  0
           | otherwise =  1

--pred :: Int -> Int
--pred (n + 1) = n

safetailV1 xs = if null xs then []
                else tail xs

safetailV2 xs | null xs = []
              | otherwise = tail xs

safetailV3 []       = []
safetailV3 (_ : xs) = xs

andV1 b1 b2 = if b1 then
                if b2 then True
                else False
              else False

andV2 b1 b2 = if b1 then
                b2
              else False
