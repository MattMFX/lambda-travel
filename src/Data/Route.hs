module Data.Route (loadRoutes) where
import Data.List.Split (splitOn)
import Geo.Route (Route, mkRoute)
import Geo.Location (Location (Location))

loadRoutes :: String -> [Location] -> IO [Route]
loadRoutes aircompany locs = do
    content <- readFile $ "data/air-companies/" <> aircompany <> ".txt"
    let routesStr = lines content
    let routes = map (mkRoute' aircompany locs) routesStr
    return routes

mkRoute' :: String -> [Location] -> String -> Route
mkRoute' aircompany locs s = mkRoute aircompany (origin `locationIn` locs) (destination `locationIn` locs) price
    where 
        params = splitOn " | " s
        price = fromInteger $ read $ last params
        route = splitOn " -> " $ head params
        origin = head route
        destination = last route

locationIn :: String -> [Location] -> Location
locationIn name locs = case location of
    [] -> error $ "Location " <> name <> " not found"
    (l:_) -> l
    where
        location = filter (\(Location n _ _) -> n == name) locs