-- myrepeat n val = take n (cycle [val])

myrepeat n = cycle [n]

subseq start end xs
  | start > end = []
  | otherwise = take (end - start) (drop start xs)

inFirstHalf x xs = x `elem` firstHalf
                      where firstHalf = take ((length xs) `div` 2) xs
