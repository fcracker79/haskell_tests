module SemigroupExample where
import Data.Semigroup

data Predicate a = Predicate { applyPredicate:: a -> Bool }

instance Semigroup u => Semigroup(Predicate u) where
    (Predicate a) <> (Predicate b) = Predicate ( \x -> (a x) || (b x) )

is_hello :: Predicate String
is_hello = Predicate (== "hello")

is_world :: Predicate String
is_world = Predicate (== "world")

is_hello_or_world :: Predicate String
is_hello_or_world = is_hello <> is_world

-- applyPredicate is_hello "hello" -- True
-- applyPredicate is_hello "hola" -- False
-- applyPredicate is_world "world" -- True
-- applyPredicate is_world "mundo" -- False
-- 
-- applyPredicate is_hello_or_world "hello" -- True
-- applyPredicate is_hello_or_world "world" -- True
-- applyPredicate is_hello_or_world "hola" -- False
-- applyPredicate is_hello_or_world "mundo" -- False
-- 
-- foldSemigroup :: Semigroup a => a -> [a] -> a
-- foldSemigroup z a = foldr (<>) z a 
-- 
-- always_false = Predicate (\x -> False)
-- is_hello_or_world_with_fold = foldSemigroup always_false [is_hello, is_world]
-- applyPredicate is_hello_or_world_with_fold "hello" -- True
-- applyPredicate is_hello_or_world_with_fold "world" -- True
-- applyPredicate is_hello_or_world_with_fold "hola" -- False
-- applyPredicate is_hello_or_world_with_fold "mundo" -- False
