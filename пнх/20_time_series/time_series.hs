--- 20.1

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

import TSData

------ 20.1.1

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

--- 20.2

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

------ 20.2.1

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

--- 20.3

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

median :: (Real a) => [a] -> Double
median xs = if odd xs_l
            then x2
            else (x1 + x2) / 2
    where xs_l = length xs
          i = xs_l `div` 2
          x1 = realToFrac (xs !! i)
          x2 = realToFrac (xs !! (i + 1))

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) =
    if all (== Nothing) values
    then Nothing
    else Just avg
  where justVals = filter isJust values
        cleanVals = map fromJust justVals
        avg = mean cleanVals

medianTS :: (Real a) => TS a -> Maybe Double
medianTS (TS _ []) = Nothing
medianTS (TS times values) =
    if all (== Nothing) values
    then Nothing
    else Just medianAverage
  where justVals = filter isJust values
        cleanVals = map fromJust justVals
        medianAverage = median cleanVals

------ 20.3.1

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where
    newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, val) = (i, val)
    newFunc (i, val) (_, Nothing) = (i, val)
    newFunc (i1, Just val1) (i2, Just val2) = if func val1 val2 == val1
                                              then (i1, Just val1)
                                              else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =
    if all (== Nothing) values
    then Nothing
    else Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

--- 20.4

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

ratioPair :: (Num a, Fractional a, Eq a) => Maybe a -> Maybe a -> Maybe a
ratioPair Nothing _ = Nothing
ratioPair _ Nothing = Nothing
-- ratioPair _ (Just 0) = Nothing
ratioPair (Just x) (Just y) = if ( y == negate y)
                              then Nothing
                              else Just (x / y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
  where shiftValues = tail values
        diffValues = zipWith diffPair shiftValues values

ratioTS :: (Num a, Fractional a, Eq a) => TS a -> TS a
ratioTS (TS [] []) = TS [] []
ratioTS (TS times values) = TS times (Nothing:ratioValues)
  where shiftValues = tail values
        ratioValues = zipWith ratioPair shiftValues values

--- 20.5
genMeanMaybe :: (Real a) => ([a] -> Double) -> [Maybe a] -> Maybe Double
genMeanMaybe f vals =
    if any (== Nothing) vals
    then Nothing
    else (Just avg)
  where avg = f (map fromJust vals)

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe = genMeanMaybe median

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n =
    if length nextVals == n
    then meanMaybe nextVals : movingAvg restVals n
    else []
  where nextVals = take n vals
        restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where ma = movingAvg values n
        nothings = replicate (n `div` 2) Nothing
        smoothedValues = mconcat [nothings, ma, nothings]

--- Extensions

applyToTS :: (a -> b) -> TS a -> TS b
applyToTS f (TS [] []) = TS [] []
applyToTS f (TS times values) = TS times (map (fmap f) values)

multiplyTS :: (a -> b -> c) -> TS a -> TS b -> TS c
multiplyTS f (TS [] []) _ = TS [] []
multiplyTS f _ (TS [] []) = TS [] []
multiplyTS f (TS t1 v1) (TS t2 v2) = TS completeTimes multipliedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tv1Map = foldl insertMaybePair Map.empty (zip t1 v1)
    tv2Map = foldl insertMaybePair Map.empty (zip t2 v2)
    f' _ Nothing = Nothing
    f' Nothing _ = Nothing
    f' (Just a) (Just b) = Just (f a b)
    multipliedValues = map (\t -> f' (Map.lookup t tv1Map) (Map.lookup t tv2Map)) completeTimes

sumTS :: Num a => TS a -> TS a -> TS a
sumTS x y = multiplyTS (+) x y

productTS :: Num a => TS a -> TS a -> TS a
productTS x y = multiplyTS (*) x y

deltaTS :: Num a => TS a -> TS a -> TS a
deltaTS x y = multiplyTS (-) x y

stdDeviation :: (Num a, Fractional a, Floating a) => TS a -> a
stdDeviation (TS [] []) = 0
stdDeviation (TS times values) = s
  where s = sqrt d
        d = sum (map (\x -> (x - avg)^2) cleanVals) / l
        avg = (sum cleanVals) / l
        l = realToFrac (length cleanVals)
        cleanVals = map fromJust justVals
        justVals = filter isJust values
