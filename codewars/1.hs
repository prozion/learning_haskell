highAndLow :: String -> String
highAndLow input = unwords [maxn, minn]
  where maxn = show (maximum numbers)
        minn = show (minimum numbers)
        numbers = map read (words input) :: [Int]
