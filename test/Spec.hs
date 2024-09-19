{-# LANGUAGE BlockArguments #-}
import Geo.Distance (Distance, mkDistance, safeDifferenceDistance, mkInfinity)
import Control.Exception (SomeException(), try, evaluate, Exception (displayException))
import GHC.IO (unsafePerformIO)
import Data.List (isInfixOf)
import Data.Data (typeOf)
import Geo.Route (Route(distance), mkRoute)
import Geo.Location (Location(..), lookupLocation)
import Geo.Path (Path(..), shortestPaths)

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
    locationLookupNonExistingLocationTest]

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
distanceSafeDifferenceTest () = ("Safe difference finite test", mkDistance 1 `safeDifferenceDistance` mkDistance 2 == mkDistance 1)

distanceSafeDifferenceInfinityTest :: () -> (String, Bool)
distanceSafeDifferenceInfinityTest () = ("Safe difference infinity test", mkInfinity `safeDifferenceDistance` mkDistance 2 == mkInfinity)

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