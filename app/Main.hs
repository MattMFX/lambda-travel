module Main (main) where
import Geo.Location (Location (Location))
import Geo.Route (mkRoute, Route (Route, distance))
import Geo.Path (mkPath, update, initialPaths, shortestPaths)
import Geo.Distance (Distance (Finite))


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

    let spr1 = mkRoute sp rj
    let spr2 = mkRoute sp cr
    let spr3 = mkRoute sp sv

    print $ distance spr3

    let routes = [route1, route2, route3, route4]
    let sprs = [spr1, spr2, spr3]

    let path = mkPath routes
    print path

    let updatedPath = update [path] (Finite 100) sprs
    print updatedPath

    print $ shortestPaths sp [sp, rj, bs, cr, sv] routes []
    
