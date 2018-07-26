module Reader where

newtype Reader e a = Reader {run :: (e -> a)}

instance Functor(Reader e) where
    fmap f (Reader r) = Reader (f . r)

