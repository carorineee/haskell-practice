module Lists (
  quicksort,
  zip',
  toUpperWord,
) where

import Data.Char (toUpper)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = (quicksort [c | c <- xs, c < x]) ++ [x] ++ (quicksort [c | c <- xs, c >= x])

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

toUpperWord :: String -> String
toUpperWord s = [toUpper c | c <- s]