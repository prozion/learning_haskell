noQueenCrosses :: [Int] -> Bool
noQueenCrosses xs =
  all (\x -> xlast /= x) xs' &&  -- no verticals
  all (\(x, y) -> (x - y) /= dback) (zip xs' backwardIdx) && -- no up diagonal
  all (\(x, y) -> (x + y) /= dforward ) (zip xs' backwardIdx) -- no down diagonal
    where xlast = head xs
          xs' = tail xs
          ymax = (length xs)
          dback = xlast - ymax
          dforward = xlast + ymax
          backwardIdx = tail [ymax, ymax-1 .. 1]

findQueenConfigurations :: Int -> [[Int]]
findQueenConfigurations 1 = map (:[]) [1..8]
findQueenConfigurations size =
  filter
    noQueenCrosses
    (mconcat $ map (\x -> map (x:) (findQueenConfigurations (size - 1))) [1..8])

main :: IO ()
main = do
  print (map reverse (findQueenConfigurations 8))
