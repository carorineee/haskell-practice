import Lists
import Conditionals
import Recursion
import HigherOrderFunc
import Types
import Data.Char

main = do 
  putStrLn "First name?"
  firstname <- getLine
  let upper = map toUpper firstname
  putStrLn $ "Hi " ++ upper