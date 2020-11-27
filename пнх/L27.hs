module L27 where

import qualified Data.Map as Map
import Data.Maybe

data RobotPart = RobotPart {
                    name :: String,
                    robot_model :: String,
                    cost :: Double,
                    count :: Int }
                deriving Show

data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box a) = Box (f a)

morePresents :: Box a -> Int -> Box [a]
morePresents box n = fmap (\ x -> (take n (repeat x))) box

myBox :: Box Int
myBox = Box 1

wrap :: a -> (Box a)
wrap a = Box a

unwrap :: (Box a) -> a
unwrap (Box a) = a

wrapped = fmap wrap myBox

parts :: [RobotPart]
parts = [
          RobotPart "arm" "T101" 65.0 5,
          RobotPart "liquid" "T1000" 120.0 100,
          RobotPart "armfire gun" "TX" 230.0 7 ]

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList (zip [1..] parts)

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "No record in the database"
printCost (Just cost) = print cost

clInterface :: IO ()
clInterface = do
  putStrLn "Input robot's ID >"
  id <- getLine
  let part = Map.lookup (read id) partsDB
  printCost (cost <$> part)
