-- Functor typeclass
-- class Functor f where  
--    fmap :: (a -> b) -> f a -> f b 
-- f is a type constructor

instance Functor Maybe where
  fmap func (Just x) = Just (func x)
  fmap func Nothing = Nothing