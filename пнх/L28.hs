import qualified Data.Map as Map
import L27

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [
  ("Arkham", (42.6054, -70.7829)),
  ("Innsmouth", (42.8250, -70.8150)),
  ("Karkoza", (29.9714, -90.7694)),
  ("New York", (40.7776, -73.9691))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where rlat = toRadians lat
        rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where (rlat1, rlong1) = latLongToRads coords1
        (rlat2, rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong/2)) ^ 2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 6378.1

haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, no code for the city"
printDistance (Just dist) = putStrLn (show dist ++ " km")

main :: IO ()
main = do
  putStrLn "Print name of the first city: "
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Print name of the second city: "
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO coords1 coords2 = haversine <$> coords1 <*> coords2

findCheaperApp :: IO ()
findCheaperApp = do
  putStrLn "First robot part:"
  id1 <- getLine
  putStrLn "Second robot part:"
  id2 <- getLine
  let rp1 = Map.lookup (read id1) partsDB
  let rp2 = Map.lookup (read id2) partsDB
  putStrLn "Cheaper part is:"
  printCheaper (findCheaper <$> rp1 <*> rp2)
    where findCheaper p1 p2 = minimum [(cost p1), (cost p2)]
          printCheaper Nothing = putStrLn "The record is not found in the Database"
          printCheaper (Just c) = putStrLn (show c)
