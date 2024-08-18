module Geo.Location (Location(..), lookupLocation) where

data Location = Location {
    name :: String,
    xCoordinate :: Int,
    yCoordinate :: Int
    }

instance Show Location where
    show (Location n _ _) = n

instance Eq Location where
    (Location n1 _ _) == (Location n2 _ _) = n1 == n2

lookupLocation :: String -> [Location] -> Maybe Location
lookupLocation locationName locs = case match of
    (l:_) -> Just l
    [] -> Nothing
    where
        match = filter (\(Location n _ _) -> n == locationName) locs