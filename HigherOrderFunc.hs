module HigherOrderFunc (
  evenHundred,
  absNeg,
) where

-- Lambdas

-- Filter for all evens between 1 and 100
evenHundred :: [Int]
evenHundred = filter (\n -> n `mod` 2 == 0) [1..100]

-- Function composition with . operator

-- Take abs then negate
absNeg :: [Int] -> [Int]
absNeg x = map (negate . abs) x