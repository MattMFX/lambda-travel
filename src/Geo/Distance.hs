module Geo.Distance (Distance, mkDistance, mkInfinity, safeDifferenceDistance) where

data Distance = Finite Double | Infinity
    deriving (Eq, Ord)

mkDistance :: Double -> Distance
mkDistance d = if d >= 0 then Finite d else error "Distance cannot be negative"

mkInfinity :: Distance
mkInfinity = Infinity

instance Num Distance where
    (Finite d1) + (Finite d2) = Finite (d1 + d2)
    _ + _ = Infinity
    (Finite d1) - (Finite d2) = if (d1 - d2) > 0 then Finite (d1 - d2) else error "Distance cannot be negative"
    _ - _ = Infinity
    (Finite d1) * (Finite d2) = Finite (d1 * d2)
    _ * _ = Infinity
    abs (Finite d) = Finite (abs d)
    abs _ = Infinity
    signum (Finite d) = Finite (signum d)
    signum _ = Infinity
    fromInteger i = Finite (fromInteger i)


instance Fractional Distance where
    (Finite d1) / (Finite d2) = if d2 /= 0 then Finite (d1 / d2) else error "Division by zero"
    _ / _ = Infinity
    fromRational r = Finite (fromRational r)

instance Show Distance where
    show (Finite d) = show d <> " km"
    show Infinity = "Infinity"

asDouble :: Distance -> Maybe Double
asDouble (Finite d) = Just d
asDouble Infinity = Nothing

safeDifferenceDistance :: Distance -> Distance -> Distance
safeDifferenceDistance d1 d2 = case ((-) <$> asDouble d1) <*> asDouble d2 of
    Just d -> mkDistance $ abs d
    _ -> Infinity
