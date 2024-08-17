module Geo.Route (mkRoute, Route(..)) where
import Geo.Location (Location (Location))

type Distance = Double

data Route = Route {
    origin :: Location,
    destination :: Location,
    distance :: Distance
    }

mkRoute :: Location -> Location -> Route
mkRoute o d = Route o d (dist o d)

dist :: Location -> Location -> Distance
dist (Location _ x1 y1) (Location _ x2 y2) =  sqrt $ (fromIntegral ct1 ^ 2) + (fromIntegral ct2 ^ 2)
    where 
        ct1 = abs (x1 - x2)
        ct2 = abs (y1 - y2)

instance Show Route where
    show (Route (Location n1 _ _) (Location n2 _ _) d) = n1 <> " -> " <> n2 <> " (" <> show d <> " km)"