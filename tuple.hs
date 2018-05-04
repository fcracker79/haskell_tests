data Tuple l a = Tuple l a deriving (Show, Eq)

get_l (Tuple l a) = l
get_a (Tuple l a) = a

:{
instance Functor(Tuple l) where
    fmap f (Tuple l a) = Tuple l (f a)
:}

let d = Tuple "Left element" 123
let dd = fmap (*2) d

print (get_l dd)
print (get_a dd)

Tuple left_element_string two_four_six = dd
