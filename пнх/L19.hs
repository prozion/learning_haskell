import qualified Data.Map as Map
import Data.Maybe

data Organ = Heart | Brain | Kidney | Spleen
  deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \ id -> Map.lookup id catalog

drawerContents = getDrawerContents possibleDrawers organCatalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers xs = (length . filter isNothing) xs

-- emptyDrawers :: [Maybe Organ] -> Int
-- emptyDrawers [] = 0
-- emptyDrawers (Nothing:xs) = 1 + emptyDrawers xs
-- emptyDrawers (x:xs) = emptyDrawers xs

res1 = emptyDrawers drawerContents

-- maybeMap :: (a -> b) -> [(Maybe a)] -> [(Maybe b)]
-- maybeMap _ [] = []
-- maybeMap f (Nothing:xs) = Nothing:(maybeMap f xs)
-- maybeMap f ((Just x):xs) = (Just (f x)):(maybeMap f xs)
--
-- maybees = [Just 1, Just 2, Just 3, Nothing, Just 5, Nothing, Nothing, Just 7]
-- res2 = maybeMap (\ x -> x^2) maybees

maybeMap :: (a -> b) -> (Maybe a) -> (Maybe b)
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)
