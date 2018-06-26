module OrdExercises where

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Ord, Show, Eq)

-- instance Eq(DayOfWeek) where
--     Mon == Mon = True
--     Tue == Tue = True
--     Wed == Wed = True
--     Thu == Thu = True
--     Fri == Fri = True
--     Sat == Sat = True
--     Sun == Sun = True
--     _ == _ = False
