module Monetary.Price (Price(..), infinitePrice, safeDifferencePrice) where

data Price = Finite Double | Infinity
    deriving (Eq, Ord)

instance Num Price where
    (Finite p1) + (Finite p2) = Finite (p1 + p2)
    _ + _ = Infinity
    (Finite p1) - (Finite p2) = if (p1 - p2) > 0 then Finite (p1 - p2) else error "Price cannot be negative"
    _ - _ = Infinity
    (Finite p1) * (Finite p2) = Finite (p1 * p2)
    _ * _ = Infinity
    abs (Finite p) = Finite (abs p)
    abs _ = Infinity
    signum (Finite p) = Finite (signum p)
    signum _ = Infinity
    fromInteger i = Finite (fromInteger i)

instance Fractional Price where
    (Finite p1) / (Finite p2) = if p2 /= 0 then Finite (p1 / p2) else error "Division by zero"
    _ / _ = Infinity
    fromRational r = Finite (fromRational r)

instance Show Price where
    show (Finite p) = "$" <> show p
    show Infinity = "Infinity"

asDouble :: Price -> Maybe Double
asDouble (Finite p) = Just p
asDouble Infinity = Nothing

safeDifferencePrice :: Price -> Price -> Price
safeDifferencePrice p1 p2 = case ((-) <$> asDouble p1) <*> asDouble p2 of
    Just p -> Finite $ abs p
    _ -> Infinity

infinitePrice :: Price
infinitePrice = Infinity

