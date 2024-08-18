module Data.Route (loadRoutes) where
import Data.List.Split (splitOn)
import Geo.Route (Route, mkRoute)
import Geo.Location (Location (Location))

loadRoutes :: [Location] -> IO [Route]
loadRoutes locs = do
    content <- readFile "data/routes.txt"
    let routesStr = lines content
    let routes = map (mkRoute' locs) routesStr
    return routes

mkRoute' :: [Location] -> String -> Route
mkRoute' locs s = mkRoute (origin `locationIn` locs) (destination `locationIn` locs)
    where 
        params = splitOn " -> " s
        origin = head params
        destination = last params

locationIn :: String -> [Location] -> Location
locationIn name locs = head $ filter (\(Location n _ _) -> n == name) locs