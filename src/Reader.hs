module Reader where
import Data.Functor

newtype Reader e a = Reader {run :: (e -> a)}

instance Functor(Reader e) where
    fmap f (Reader r) = Reader (f . r)

