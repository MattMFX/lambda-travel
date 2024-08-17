module Geo.Path where

import Geo.Location (Location (Location, name))
import Geo.Route (mkRoute, Route (Route, origin))

data Path = Path {
    routes :: [Route],
    totalDistance :: Double
    }

-- Usar a definicao de path para calcular a o menor caminho!!!
-- Criar funcao traverse

instance Show Path where
    show (Path rs d) = unlines $ map show rs <> ["Total distance: " <> show d]

data Distance = Finite Double | Infinity
    deriving (Eq, Ord, Show)

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

-- shortestPath :: [Location] -> [Route] -> Location -> Location -> Maybe Path
-- shortestPath [] _ _ _ = Nothing
-- shortestPath _ [] _ _ = Nothing
-- shortestPath ls rs o d = Just $ Path (shortestPath' ls rs o d) 0

-- routesStartingAt :: Location -> [Route] -> [Route]
-- routesStartingAt l = filter (\(Route o _ _) -> o == l)

-- -- routeOriginDestination :: Route -> (String, String)
-- -- routeOriginDestination (Route (Location n1 _ _) (Location n2 _ _) _) = (n1, n2)

-- initialDistances :: [Location] -> Location -> [(Location, Distance)]
-- initialDistances ls l = map toDistances ls
--     where
--         toDistances l' = (l', if l == l' then Finite 0 else Infinity)

-- updateDistance :: [Route] -> (Location, Distance) -> (Location, Distance)
-- updateDistance (r:rs) (l, ds) = case r of
--     Route o d d' -> if o == l then (d, Finite d') else (l, ds)

-- updateDistance :: Route -> (Location, Distance) -> (Location, Distance)
-- updateDistance r (d, ds) = case r of
--     Route o d' ds' -> if d' == d then (d, Finite smallest) else (l, ds)
--         where
--             smallest = case ds of
--                 Finite d'' -> min d'' d'
--                 Infinity -> d'

-- distanceUsingRoute :: Route -> Distance
-- distanceUsingRoute (Route _ _ d) = Finite d

-- distanceOf :: [(Location, Distance)] -> String -> Distance
-- distanceOf ds s = snd $ head $ filter (\(l, _) -> name l == s) ds

-- traverse :: Location -> [Route] -> [(Location, Distance)] -> [(Location, Distance)]
-- traverse l (r:rs) ds = update r ds
--     where
--         thisDistance = distanceOf l ds

-- destinationDistance :: Route -> Distance
-- destinationDistance (Route _ _ d') = Finite d'

-- distanceOf :: Location -> [(Location, Distance)] -> Distance
-- distanceOf l = snd . head . filter (\(l', _) -> l == l')

-- update :: [Route] -> [(Location, Distance)] -> [(Location, Distance)]
-- update [] ds = ds
-- update [r] ds = map (update' (origin r)) ds
--     where 
--         update' l (l', ds') = if l' == l then updateDistance l (distanceOf l ds) (ds') () else (l', ds')
-- update (r:rs) ds = update rs (update [r] ds)

-- updateDistance :: Location -> Distance -> Distance -> Distance -> (Location, Distance)
-- updateDistance d1 d2 d3 = if d1 + d2 < d3 then d1 + d2 else d3