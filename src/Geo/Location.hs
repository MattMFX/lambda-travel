module Geo.Location (Location(..)) where

data Location = Location {
    name :: String,
    xCoordinate :: Int,
    yCoordinate :: Int
    }