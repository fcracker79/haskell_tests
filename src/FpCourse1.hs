-- Functors

module FpCourse1 where

import Data.Functor

double :: (Num a, Show a) => Maybe a -> Maybe a
double a = Data.Functor.fmap (* 2) a

plus_1 :: (Num a, Show a) => Maybe a -> Maybe a
plus_1 a = Data.Functor.fmap (+ 1) a

-- Maybe is a functor
inverse :: (Fractional a, Show a, Eq a) => Maybe a -> Maybe a
inverse (Just a) = if a /= 0 then Just (1 / a) else Nothing
inverse Nothing = Nothing

double_plus_1 :: (Num a, Show a) => Maybe a -> Maybe a
double_plus_1 = double . plus_1

plus_1_double :: (Num a, Show a) => Maybe a -> Maybe a
plus_1_double = plus_1 . double

data MyTuple l a = MyTuple l a

instance Functor(MyTuple l) where
  fmap f (MyTuple l a) = MyTuple l (f a)
