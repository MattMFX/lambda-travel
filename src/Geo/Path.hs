module Geo.Path (Path(..), mkPath, update, initialPaths, shortestPaths) where

import Geo.Location (Location)
import Geo.Route (Route (Route, origin, destination))
import Geo.Distance (Distance(Infinity))
import Data.List (delete, sort)
import Text.ParserCombinators.ReadP (get)

data Path = Path {
    pathOrigin :: Location,
    pathDestination :: Location,
    pathRoutes :: [Route],
    totalDistance :: Distance
    }

instance Eq Path where
    (Path _ _ _ d1) == (Path _ _ _ d2) = d1 == d2

instance Ord Path where
    (Path _ _ _ d1) <= (Path _ _ _ d2) = d1 <= d2
    (Path _ _ _ d1) >= (Path _ _ _ d2) = d1 >= d2

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
shortestPaths _ [] _ skp = skp
shortestPaths loc locs rs skp = shortestPaths (getNextLocation locs sortedPaths) unvisitedLocations rs updatedPaths
    where 
        unvisitedLocations = delete loc locs
        updatedPaths = update skp (totalDistance $ pathTo loc skp) (routesStartingAt loc rs)
        sortedPaths = sort updatedPaths


update :: ShortestKnownPaths -> CurrentDistance -> Routes -> ShortestKnownPaths
update skp _ [] = skp
update skp currentDistance ((Route _ rtDestination rtDistance):rs) = map update' skp'
    where
        skp' = update skp currentDistance rs
        knownDistance = totalDistance $ pathTo rtDestination skp'
        newDistance = currentDistance + rtDistance
        isShorter = newDistance < knownDistance
        updatedDistance = if isShorter then newDistance else knownDistance
        update' (Path o d r td)
            | o == d = Path o d r 0
            | otherwise = if d == rtDestination then Path o d r updatedDistance else Path o d r td

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

getNextLocation :: Unvisited -> ShortestKnownPaths -> Location
getNextLocation [] _ = error "No more locations to visit"
getNextLocation _ [] = error "No more paths to check"
getNextLocation ls (p:ps) 
    | pathDestination p `elem` ls = pathDestination p
    | otherwise = getNextLocation ls ps