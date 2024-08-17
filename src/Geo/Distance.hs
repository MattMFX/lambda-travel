module Geo.Distance (Distance(..)) where

data Distance = Finite Double | Infinity
    deriving (Eq, Ord)

instance Num Distance where
    (Finite d1) + (Finite d2) = Finite (d1 + d2)
    _ + _ = Infinity
    (Finite d1) - (Finite d2) = Finite (d1 - d2)
    _ - _ = Infinity
    (Finite d1) * (Finite d2) = Finite (d1 * d2)
    _ * _ = Infinity
    abs (Finite d) = Finite (abs d)
    abs _ = Infinity
    signum (Finite d) = Finite (signum d)
    signum _ = Infinity
    fromInteger i = Finite (fromInteger i)

instance Show Distance where
    show (Finite d) = show d <> " km"
    show Infinity = "Infinity"