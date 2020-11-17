import qualified Data.Map as Map
import L8

helloPerson :: String -> String
helloPerson name = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter diameter = pi * (diameter / 2) ^ 2

costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / areaGivenDiameter size

describePizza :: Pizza -> String
describePizza (size, cost) = "Pizza of size " ++ show size ++ " is cheaper with price " ++ show costSqCm ++ " per square cm"
  where costSqCm = costPerCm (size, cost)

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
  where costP1 = costPerCm p1
        costP2 = costPerCm p2

costData :: Map.Map Int Double
costData = Map.fromList [(1, 150), (2, 220)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 30), (2, 50)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

--- 21.1

inputData :: Map.Map String String
inputData = Map.fromList[("name", "Denis")]

main2 :: Maybe String
main2 = do
  name <- Map.lookup "name" inputData
  let statement = helloPerson name
  return statement

--- 21.2

main3 :: IO ()
main3 = do
  putStrLn "Please, input n to calculate fib(n):"
  input <- getLine
  let n = read input
  putStrLn ("ans = " ++ show (fib n))
