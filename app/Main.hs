module Main (main) where
import Geo.Location (Location (Location))
import Geo.Route (mkRoute)


main :: IO ()
main = do
    let sp = Location "Sao Paulo" 0 0
    let rj = Location "Rio de Janeiro" 200 (-310)
    let bs = Location "Brasilia" (-431) 1142
    let cr = Location "Curitiba" 134 (-672)
    let sv = Location "Salvador" 231 2455

    let route1 = mkRoute sp rj
    let route2 = mkRoute rj bs
    let route3 = mkRoute bs cr
    let route4 = mkRoute cr sv
    let route5 = mkRoute sv sp

    let routes = [route1, route2, route3, route4, route5]
    mapM_ print routes
