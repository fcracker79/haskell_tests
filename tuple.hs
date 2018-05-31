module TupleFunctor where

data Tuple l a = Tuple l a deriving (Show, Eq)

get_l :: Tuple x y -> x
get_l (Tuple l _) = l

get_a :: Tuple x y -> y
get_a (Tuple _ a) = a

instance Functor(Tuple l) where
    fmap f (Tuple l a) = Tuple l (f a)

d :: Tuple String Integer
d = Tuple "Left element" 123

dd :: Tuple String Integer
dd = fmap (*2) d

left_element_string :: String
two_four_six :: Integer
Tuple left_element_string two_four_six = dd
