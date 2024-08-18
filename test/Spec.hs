{-# LANGUAGE BlockArguments #-}
import Geo.Distance (Distance, mkDistance, safeDifference, mkInfinity)
import Control.Exception (SomeException(), try, evaluate, Exception (displayException))
import GHC.IO (unsafePerformIO)
import Data.List (isInfixOf)
import Data.Data (typeOf)
import Geo.Route (Route(distance), mkRoute)
import Geo.Location (Location(..), lookupLocation)
import Geo.Path (Path(..), mkPath, pathTo, shortestPaths)

main :: IO ()
main = do testRunner
    [distanceMkDistanceTest,
    distanceSumTest,
    distanceNegativeTest,
    distanceDifferenceTest,
    distanceMultiplicationTest,
    distanceAbsTest,
    distanceSignumTest,
    distanceFromIntegerTest,
    distanceSafeDifferenceTest,
    distanceSafeDifferenceInfinityTest,
    locationLookupExistingLocationTest,
    locationLookupNonExistingLocationTest,
    routeMkRouteTest,
    routeCalculateDistanceTest,
    pathMkPathTest,
    pathPathToTest,
    pathShortestPathsTest]

testRunner :: [() -> (String, Bool)] -> IO ()
testRunner = mapM_ (\f -> if testResult f then putStrLn $ "Test " <> testCase f <> " success" else error $ "Test " <> testCase f <> " failed")
    where
        testCase test = fst $ test ()
        testResult test = snd $ test ()

distanceMkDistanceTest :: () -> (String, Bool)
distanceMkDistanceTest () = ("Distance creation test", show (typeOf (mkDistance 1)) == "Distance")

distanceSumTest :: () -> (String, Bool)
distanceSumTest () = ("Distance test", (mkDistance 1 + mkDistance 2) == mkDistance 3)

distanceNegativeTest :: () -> (String, Bool)
distanceNegativeTest () = ("Negative distance test", result)
    where
        result = case unsafePerformIO (try (evaluate (mkDistance 1 - mkDistance 2)) :: IO (Either SomeException Distance)) of
            Left ex -> "Distance cannot be negative" `isInfixOf` displayException ex
            Right _ -> False

distanceDifferenceTest :: () -> (String, Bool)
distanceDifferenceTest () = ("Distance difference test", (mkDistance 2 - mkDistance 1) == mkDistance 1)

distanceMultiplicationTest :: () -> (String, Bool)
distanceMultiplicationTest () = ("Distance multiplication test", (mkDistance 2 * mkDistance 3) == mkDistance 6)

distanceAbsTest :: () -> (String, Bool)
distanceAbsTest () = ("Distance abs test", abs (mkDistance 1) == mkDistance 1)

distanceSignumTest :: () -> (String, Bool)
distanceSignumTest () = ("Distance signum test", signum (mkDistance 1) == mkDistance 1)

distanceFromIntegerTest :: () -> (String, Bool)
distanceFromIntegerTest () = ("Distance from integer test", 1 == mkDistance 1)

distanceSafeDifferenceTest :: () -> (String, Bool)
distanceSafeDifferenceTest () = ("Safe difference finite test", mkDistance 1 `safeDifference` mkDistance 2 == mkDistance 1)

distanceSafeDifferenceInfinityTest :: () -> (String, Bool)
distanceSafeDifferenceInfinityTest () = ("Safe difference infinity test", mkInfinity `safeDifference` mkDistance 2 == mkInfinity)

locationLookupExistingLocationTest :: () -> (String, Bool)
locationLookupExistingLocationTest () =
    (
        "Existing location lookup test",
        case lookupLocation "C" locationList of
            Just (Location "C" _ _) -> True
            _ -> False
    )
    where
        locationList = [Location "A" 0 0, Location "B" 1 1, Location "C" 2 2]

locationLookupNonExistingLocationTest :: () -> (String, Bool)
locationLookupNonExistingLocationTest () =
    (
        "Non-existing location lookup test",
        case lookupLocation "D" locationList of
            Nothing -> True
            _ -> False
    )
    where
        locationList = [Location "A" 0 0, Location "B" 1 1, Location "C" 2 2]

routeMkRouteTest :: () -> (String, Bool)
routeMkRouteTest () = ("Route creation test", show (typeOf (mkRoute (Location "A" 0 0) (Location "B" 1 1))) == "Route")

routeCalculateDistanceTest :: () -> (String, Bool)
routeCalculateDistanceTest () = ("Route distance calculation test", safeDifference expectedDistance foundDistance < 0.0000001)
    where
        expectedDistance = mkDistance 3284.6153503873
        foundDistance = distance (mkRoute (Location "A" 2311 (-1)) (Location "B" (-972) 102))

pathMkPathTest :: () -> (String, Bool)
pathMkPathTest () = do ("Path creation test", expectedType && expectedDestination && expectedOrigin && expectedDistance)
    where
        expectedType = show (typeOf path) == "Path"
        expectedOrigin = pathOrigin path == location "A"
        expectedDestination = pathDestination path == location "E"
        expectedDistance = calculatedDistances `safeDifference` totalDistance path < 0.0000001
        path = mkPath routes
        locations = [Location "A" 2311 (-1), Location "B" (-972) 102, Location "C" (-7622) (-2701), Location "D" 681 6200, Location "E" (-10111) 9622]
        routes =
            [
                mkRoute (location "A") (location "B"),
                mkRoute (location "B") (location "C"),
                mkRoute (location "C") (location "D"),
                mkRoute (location "D") (location "E")
            ]
        calculatedDistances =
            distance (mkRoute (location "A") (location "B")) +
            distance (mkRoute (location "B") (location "C")) +
            distance (mkRoute (location "C") (location "D")) +
            distance (mkRoute (location "D") (location "E"))
        location n = case lookupLocation n locations of
            Just l -> l
            _ -> error "Location not found"

pathPathToTest :: () -> (String, Bool)
pathPathToTest () = ("Path to test", expectedPath == pathTo (location "E") paths)
    where
        expectedPath = mkPath [mkRoute (location "D") (location "E")]
        locations = [Location "A" 2311 (-1), Location "B" (-972) 102, Location "C" (-7622) (-2701), Location "D" 681 6200, Location "E" (-10111) 9622]
        paths =
            [
                mkPath [mkRoute (location "A") (location "B")],
                mkPath [mkRoute (location "B") (location "C")],
                mkPath [mkRoute (location "C") (location "D")],
                expectedPath
            ]
        location n = case lookupLocation n locations of
            Just l -> l
            _ -> error "Location not found"

pathShortestPathsTest :: () -> (String, Bool)
pathShortestPathsTest () = ("Shortest paths test", match expectedPaths $ shortestPaths (location "A") locations routes)
    where
        expectedPaths =
            [
                mkPath [mkRoute (location "A") (location "B")],
                mkPath [mkRoute (location "A") (location "B"), mkRoute (location "B") (location "C")],
                mkPath [mkRoute (location "A") (location "B"), mkRoute (location "B") (location "C"), mkRoute (location "C") (location "D")],
                mkPath [mkRoute (location "A") (location "B"), mkRoute (location "B") (location "E")]
            ]
        locations = [Location "A" 2311 (-1), Location "B" (-972) 102, Location "C" (-7622) (-2701), Location "D" 681 6200, Location "E" (-10111) 9622]
        routes =
            [
                mkRoute (location "A") (location "B"),
                mkRoute (location "B") (location "C"),
                mkRoute (location "C") (location "D"),
                mkRoute (location "D") (location "E"),
                mkRoute (location "B") (location "E")
            ]
        location n = case lookupLocation n locations of
            Just l -> l
            _ -> error "Location not found"
        match expecteds founds = all (\(e, f) -> e == f) $ zip expecteds founds