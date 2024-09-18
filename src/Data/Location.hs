module Data.Location (loadLocations) where
import Geo.Location (Location(..))

loadLocations :: IO [Location]
loadLocations = do
    content <- readFile "data/locations.txt"
    let locationsStr = lines content
    let locations = map mkLocation locationsStr
    return locations

mkLocation :: String -> Location
mkLocation s = case params of
    [locationName, x, y] -> Location locationName (read x) (read y)
    _ -> error "Invalid location"
    where
        params = words s