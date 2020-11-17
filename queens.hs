butlast = reverse . tail . reverse

noVertical :: [Int] -> Bool
noVertical ns = not ((last ns) `elem` (butlast ns))

noDiagonal :: [Int] -> Bool
noDiagonal ns =
  all (\(n, i) -> (n - i) /= dback) (zip ns' [1..]) &&
  all (\(n, i) -> (n + i) /= dforward ) (zip ns' [1..])
    where n0 = last ns
          ns' = butlast ns
          dback = n0 - (length ns)
          dforward = n0 + (length ns)

noQueenCrosses :: [Int] -> Bool
noQueenCrosses ns = (noVertical ns) && (noDiagonal ns)

findQueenConfigurations :: Int -> [[Int]]
findQueenConfigurations 1 = map (:[]) [1..8]
findQueenConfigurations size =
  filter
    noQueenCrosses
    (mconcat $ map (\x -> map (++ [x]) (findQueenConfigurations (size - 1))) [1..8])

main :: IO ()
main = do
  print $ findQueenConfigurations 8
