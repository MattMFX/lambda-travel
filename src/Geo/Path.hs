module Geo.Path (Path(..), mkPath, update, initialPaths, shortestPaths) where

import Geo.Location (Location)
import Geo.Route (Route (Route, origin, destination))
import Geo.Distance (Distance(Infinity))

data Path = Path {
    pathOrigin :: Location,
    pathDestination :: Location,
    pathRoutes :: [Route],
    totalDistance :: Distance
    }

instance Show Path where
    show (Path o d [] td) = "No path found from " <> show o <> " to " <> show d <> " (" <> show td <> ")"
    show (Path _ _ (r:rs) ds) = show (origin r) <> " -> " <> concatPath rs <> " (Total distance: " <> show ds <> ")"
        where
            concatPath [] = ""
            concatPath [Route _ d' _] = show d'
            concatPath ((Route _ d' _):rs') = show d' <> " -> " <> concatPath rs'

mkPath :: [Route] -> Path
mkPath rs = Path (origin $ head rs) (destination $ last rs) rs (foldr (\(Route _ _ d) acc -> acc + d) 0 rs)


type Unvisited = [Location]
type Routes = [Route]
type ShortestKnownPaths = [Path]
type CurrentDistance = Distance

shortestPaths :: Location -> Unvisited -> Routes -> ShortestKnownPaths -> [Path]
shortestPaths loc locs rs [] = shortestPaths loc locs rs $ initialPaths loc locs
shortestPaths loc locs rs skp = update skp (totalDistance $ pathTo loc skp) (routesStartingAt loc rs)

update :: ShortestKnownPaths -> CurrentDistance -> Routes -> ShortestKnownPaths
update skp _ [] = skp
update skp currentDistance ((Route _ rtDestination rtDistance):rs) = map update' skp'
    where
        skp' = update skp currentDistance rs
        knownDistance = totalDistance $ pathTo rtDestination skp'
        newDistance = currentDistance + rtDistance
        isShorter = newDistance < knownDistance
        updatedDistance = if isShorter then newDistance else knownDistance
        update' (Path o d r td) = if d == rtDestination then Path o d r updatedDistance else Path o d r td

pathTo :: Location -> ShortestKnownPaths -> Path
pathTo l [] = Path l l [] 0
pathTo l paths = case fPaths paths of
    [] -> Path l l [] 0
    (p:_) -> p
    where
        fPaths = filter (\(Path _ d _ _) -> d == l)

initialPaths :: Location -> Unvisited -> ShortestKnownPaths
initialPaths l = map (\l' -> Path l l' [] Infinity)

routesStartingAt :: Location -> [Route] -> [Route]
routesStartingAt l = filter (\(Route o _ _) -> o == l)

-- applyFor :: a -> [a] -> (a -> a) -> [a]
-- applyFor a' (a:as) f = if a == a' then f a : as else a : applyFor a' as f


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