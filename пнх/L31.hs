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

main :: IO ()
main =
  putStrLn "Size of the first pizza"
  >> getLine
  >>= (\ size1 ->
      putStrLn "Price of the first pizza"
      >> getLine
      >>= (\ cost1 ->
          putStrLn "Size of the first pizza"
          >> getLine
          >>= (\ size2 ->
              putStrLn "Price of the second pizza"
              >> getLine
              >>= (\ cost2 ->
                    (\ pizza1 ->
                      (\ pizza2 ->
                        (\ betterPizza ->
                          putStrLn (describePizza betterPizza))
                        (comparePizzas pizza1 pizza2)
                      ) (read size2, read cost2)
                    ) (read size1, read cost1)
                  ))))

monadeMain :: Monad m => m Pizza -> m Pizza -> m String
monadeMain p1 p2 = do
  pizza1 <- p1
  pizza2 <- p2
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
