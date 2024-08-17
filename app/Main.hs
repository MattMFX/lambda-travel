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

    let routeSpSp = mkRoute sp sp
    let routeSpRj = mkRoute sp rj
    let routeRjBs = mkRoute rj bs
    let routeBsCr = mkRoute bs cr
    let routeCrSv = mkRoute cr sv
    let routeSpSv = mkRoute sp sv

    let routes = [routeSpSp, routeSpRj, routeRjBs, routeBsCr, routeCrSv, routeSpSv]

    print routeSpSp
    print routeSpRj
    print routeRjBs
    print routeBsCr
    print routeCrSv
    print routeSpSv

    print $ shortestPaths sp [sp, rj, bs, cr, sv] routes []
    
