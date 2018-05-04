data Reader ff = Reader ff
instance Functor(Reader) where
    fmap :: ( (e -> a) ff) => f (Reader ff ) = f . ff

data Reader ff e a = Reader ff e a
instance Functor(Reader ff e) where
    fmap f (Reader ff e) = (f . ff) e


type Reader e a = e -> a
instance Functor(Reader) where
    fmap f (Reader ff ) = f . ff


data Reader e a = (->) e a
instance Functor(Reader e) where
    fmap f (Reader ff e a) = ff e f(a)

newtype Reader e a = Reader { read :: e -> a }
instance Functor(Reader e) where
    fmap f (Reader app) = Reader (f . app)
