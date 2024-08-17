module Geo.Location (Location(..)) where

data Location = Location {
    name :: String,
    xCoordinate :: Int,
    yCoordinate :: Int
    }

instance Show Location where
    show (Location n _ _) = n

instance Eq Location where
    (Location n1 _ _) == (Location n2 _ _) = n1 == n2