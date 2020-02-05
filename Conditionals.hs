module Conditionals (
  grade,
  grades,
  initials,
  cylinderArea,
  grade',
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

initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
  where (f:_) = first
        (l:_) = last

-- Let .. in
cylinderArea :: (RealFloat a) => a -> a -> a
cylinderArea r h =
  let circleArea = pi * r^2
      sideArea = 2 * pi * r * h
  in circleArea + sideArea

-- Case Expressions
grade' :: (RealFloat a) => a -> a -> String
grade' correct questions = case proportion of 0.9 -> "A"
                                              0.8 -> "B"
                                              0.7 -> "C"
                            where proportion = correct / questions

