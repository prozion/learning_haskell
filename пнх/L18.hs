import qualified Data.Map as Map
import Data.Maybe

data Box a = Box a deriving Show
data Triple a = Triple a a a deriving Show

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

data Organ = Heart | Brain | Kidney | Spleen
  deriving (Show, Eq, Ord)

-- organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map.Map Organ Int
organInventory = Map.foldrWithKey
                    (\ k v a -> let oldCount = maybe 0 id (Map.lookup v a)
                                    newCount = oldCount + 1
                                in Map.insert v newCount a)
                    Map.empty
                    organCatalog
