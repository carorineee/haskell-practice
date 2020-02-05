module Types (
  Point(..),
  Shape(..),
  area,
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rect Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = 2 * pi * r
area (Rect (Point x1 y1) (Point x2 y2)) = abs (y2 - y1) * abs (x2 - x1)

-- Record syntax
data Person = Person {
  first :: String,
  last :: String,
  age :: Int
} deriving (Show)
