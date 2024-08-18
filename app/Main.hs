module Main (main) where
import Geo.Location (lookupLocation, Location)
import Geo.Path (shortestPaths, pathTo)
import Data.Location (loadLocations)
import Data.Route (loadRoutes)


main :: IO ()
main = do
    locations <- loadLocations
    routes <- loadRoutes locations

    putStrLn "Select a location to start from:"
    putStrLn $ unlines $ map show locations
    origin <- readLocation locations

    putStrLn "Type [1] to select a destination or type [2] to see all available destinations:"
    option <- readOption ["1", "2"]
    if option == "1"
        then do 
            putStrLn "Select a destination:"
            destination <- readLocation locations
            print $ pathTo destination $ shortestPaths origin locations routes
        else do
            putStrLn "Available destinations:"
            print $ shortestPaths origin locations routes

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