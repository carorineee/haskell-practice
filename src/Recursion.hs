module Recursion (
  length',
  take',
) where

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

take' :: (Num a, Eq a) => [a] -> a -> [a]
take' _ 0 = []
take' [] _ = []
take' (x:xs) count = x:(take' xs (count-1)) 