module Geo.Path (Path(..), shortestPaths, Cost(..)) where

import Control.Monad.State
    ( gets, evalState, execState, MonadState(get, put), State )
import Geo.Location (Location)
import Geo.Route (Route (Route, destination, distance, price))
import Geo.Distance (Distance, mkInfinity, safeDifferenceDistance)
import Data.List (delete, sort)
import Monetary.Price (infinitePrice, safeDifferencePrice, Price)

class (Show a, Num a, Fractional a, Ord a) => Cost a where
    fCost :: Route -> a
    safeDiff :: a -> a -> a
    zeroCost :: a
    infiniteCost :: a

instance Cost Distance where
    fCost = distance
    safeDiff = safeDifferenceDistance
    zeroCost = 0
    infiniteCost = mkInfinity

instance Cost Price where
    fCost = price
    safeDiff = safeDifferencePrice
    zeroCost = 0
    infiniteCost = infinitePrice

data Cost a => Path a = Path {
    pathOrigin :: Location,
    pathDestination :: Location,
    pathRoutes :: [Route],
    cost :: a
    }

instance Cost a => Eq (Path a) where
    (Path o d _ c1) == (Path o' d' _ c2) = c1 `safeDiff` c2 < 0.0000001 && o == o' && d == d'

instance Cost a => Ord (Path a) where
    (Path _ _ _ c1) <= (Path _ _ _ c2) = c1 <= c2
    (Path _ _ _ c1) >= (Path _ _ _ c2) = c1 >= c2

instance Cost a => Show (Path a) where
    show (Path o d [] _) = "No path found from " <> show o <> " to " <> show d <> "\n"
    show (Path o d rs c) = "The best path from " <> show o <> " to " <> show d <> " is:\n" <> concatPath rs <> " (Total: " <> show c <> ")" <> "\n"
        where
            concatPath [] = ""
            concatPath [r'] = "    " <> show r' <> "\n"
            concatPath (r':rs') = "    " <> show r' <> "\n" <> concatPath rs'

data ExecutionState a = ExecutionState {
    currentLocation :: Location,
    unvisitedLocations :: [Location],
    routes :: [Route],
    shortestKnownPaths :: [Path a],
    costFunction :: Route -> a
}

shortestPaths :: Cost a => Location -> [Location] -> [Route] -> (Route -> a) -> [Path a]
shortestPaths l ls rs f = sort $ compute shortestKnownPaths
    where
        compute g = g $ execState (visitNode $ Just l) (initialState l ls rs f)


visitNode :: Cost a => Maybe Location -> State (ExecutionState a) ()
visitNode Nothing = return ()
visitNode (Just l) = do
    s <- get
    put s { currentLocation = l }
    updatedPaths <- mapM updatePath (shortestKnownPaths s)
    put s { shortestKnownPaths = updatedPaths, unvisitedLocations = delete l (unvisitedLocations s) }
    next <- nextLocation
    visitNode next

updatePath :: Cost a => Path a -> State (ExecutionState a) (Path a)
updatePath p = do
    s <- get
    if routeExists s
        then do
            let knownDistance = cost $ pathToDestination p s
            let newDistance = cost (pathToCurrentLocation s) + costFunction s (routeToDestination s)
            let isShorter = newDistance < knownDistance
            let updatedDistance = if isShorter then newDistance else knownDistance
            let updatedRoutes = if isShorter then pathRoutes (pathToCurrentLocation s) <> [routeToDestination s] else pathRoutes p
            let updatedPath = if isShorter then Path (pathOrigin p) (pathDestination p) updatedRoutes updatedDistance else p
            return updatedPath
        else return p
        where
            pathToDestination p' = evalState (pathTo (pathDestination p'))
            pathToCurrentLocation s = evalState (pathTo (currentLocation s)) s
            routeExists s = pathDestination p `elem` map destination (routesFromCurrentLocation s)
            routeToDestination s = evalState (currentLocation s *->* pathDestination p) s
            routesFromCurrentLocation s = evalState (routesFrom (currentLocation s)) s

nextLocation :: Cost a => State (ExecutionState a) (Maybe Location)
nextLocation = do
    s <- get
    let next = filter (unvisited s) (sortedPaths s)
    case next of
        [] -> return Nothing
        (p:_) -> return $ Just $ pathDestination p
    where
        unvisited s (Path _ d _ _) = d `elem` unvisitedLocations s
        sortedPaths s = sort $ shortestKnownPaths s

pathTo :: Cost a => Location -> State (ExecutionState a) (Path a)
pathTo l = do
    s <- get
    let path = filter (\(Path _ d _ _) -> d == l) $ shortestKnownPaths s
    case path of
        [] -> error $ "No path found" <> show l
        (p:_) -> return p

routesFrom :: Location -> State (ExecutionState a) [Route]
routesFrom l = do
    gets (filter (\(Route _ o _ _ _) -> o == l) . routes)

(*->*) :: Location -> Location -> State (ExecutionState a) Route
(*->*) o d = do
    s <- get
    let route = filter (\(Route _ o' d' _ _) -> o' == o && d' == d) (routes s)
    case route of
        [] -> error "No route found"
        (r:_) -> return r

initialState :: Cost a => Location -> [Location] -> [Route] -> (Route -> a) -> ExecutionState a
initialState origin locations routeList = ExecutionState origin locations routeList initialPaths
    where
        initialPaths = map toPath locations
        toPath l = if l == origin
            then Path l l [] zeroCost
            else Path origin l [] infiniteCost