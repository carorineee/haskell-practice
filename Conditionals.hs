module Conditionals (
  grade,
  grades,
) where
  
-- Guards
grade :: (RealFloat a) => a -> a -> String
grade correct questions
  | proportion >= 0.9 = "A"
  | proportion >= 0.8 = "B"
  | proportion >= 0.7 = "C"
  | otherwise = "D"
  where proportion = correct / questions

grades :: (RealFloat a) => [(a, a)] -> [String]
grades xs = [grade x y | (x, y) <- xs]