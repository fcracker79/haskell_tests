module Calendar where

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
instance Eq DayOfWeek where
    Sun == Sun = True
    Mon == Mon = True
    Tue == Tue = True
    Wed == Wed = True
    Thu == Thu = True
    Fri == Fri = True
    Sat == Sat = True
    _ == _ = False

instance Eq Month where
    Jan == Jan = True
    Feb == Feb = True
    Mar == Mar = True
    Apr == Apr = True
    May == May = True
    Jun == Jun = True
    Jul == Jul = True
    Aug == Aug = True
    Sep == Sep = True
    Oct == Oct = True
    Nov == Nov = True
    Dec == Dec = True
    _   == _   = False


data Date = Date Int DayOfWeek Month
instance Eq Date where Date a b c == Date x y z = a == x && b == y && c == z

