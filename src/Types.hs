module Types (
  Point(..),
  Shape(..),
  area,
  Vector(..),
  vplus,
  Tree(..),
  treeInsert,
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

-- Type constructor
-- When data type acts as a "box"
data Maybe a = Nothing | Just a

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector q p l) = Vector (i+q) (j+p) (k+l)

-- Derived instances
-- Ex: deriving (Eq) -> will compare value constructors and go through and call == on each field

-- By default, the first value constructor is the smallest
-- data Bool = False | True deriving (Ord)  

-- Type synonyms
-- Pretty much the same as typedef in C
-- type String = [char]

type AssocList k v = [(k,v)]

-- Recursive types
data List a = Empty | Cons a (List a)

data Tree a = EmptyTree | Node a (Tree a) (Tree a)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert EmptyTree t = singleton t
treeInsert (Node t left right) x
  | x == t = Node x left right
  | x < t = Node t (treeInsert left x) right
  | x > t = Node t left (treeInsert right x)