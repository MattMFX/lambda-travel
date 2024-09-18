module Main (main) where
import Geo.Location (lookupLocation, Location)
import Geo.Path (shortestPaths, Path (Path), Cost (fCost))
import Data.Location (loadLocations)
import Data.Route (loadRoutes)
import Geo.Route (Route(price))
import Data.AirCompanies (loadAirCompanies)
import Monetary.Price (Price)
import Geo.Distance (Distance)


main :: IO ()
main = do
    locations <- loadLocations
    airCompanies <- loadAirCompanies
    let calculateRoutes = map loadRoutes airCompanies
    routesByCompany <- mapM (\f -> f locations) calculateRoutes

    putStrLn "Select a location to start from:"
    putStrLn $ unlines $ map show locations
    origin <- readLocation locations

    putStrLn "Type [1] to select a destination or type [2] to see all available destinations:"
    destinationOption <- readOption ["1", "2"]

    if destinationOption == "1"
        then do
            putStrLn "Select a destination:"
            d <- readLocation locations

            putStrLn "Type [1] to sort by best prices or [2] to sort by shortest distances:"
            sortOption <- readOption ["1", "2"]
            putStrLn "Available destinations:"
            if sortOption == "1"
                then do
                    let paths = shortestPaths origin locations (routes routesByCompany) (fCost :: Geo.Route.Route -> Price)
                    let path = filter (matchingDestination d) paths
                    print path
                else do
                    let paths = shortestPaths origin locations (routes routesByCompany) (fCost :: Geo.Route.Route -> Distance)
                    let path = filter (matchingDestination d) paths
                    print path
        else do
            putStrLn "Type [1] to sort by best prices or [2] to sort by shortest distances:"
            sortOption <- readOption ["1", "2"]
            if sortOption == "1"
                then do
                    putStrLn "Available destinations:"
                    print $ shortestPaths origin locations (routes routesByCompany) (fCost :: Geo.Route.Route -> Price)
                else do
                    putStrLn "Available destinations:"
                    print $ shortestPaths origin locations (routes routesByCompany) (fCost :: Geo.Route.Route -> Distance)
        where
            matchingDestination d (Path _ d' _ _) = d == d'
            routes = concat

readLocation :: [Location] -> IO Location
readLocation locations = do
    input <- getLine
    let location = lookupLocation input locations
    case location of
        Just l -> return l
        _ -> putStrLn "Invalid location, please enter a valid location from the list:\n" >> readLocation locations

readOption :: [String] -> IO String
readOption options = do
    input <- getLine
    if input `elem` options
        then return input
        else putStrLn "Invalid option, please enter a valid option:" >> readOption options