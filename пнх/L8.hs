module L8 where

  myDrop 0 xs = xs
  myDrop _ [] = []
  myDrop n (x:xs) = myDrop (n-1) xs

  indexof el xs = indexofn el xs 0
                  where indexofn el [] _ = -1
                        indexofn el (x:xs) n
                          | el == x = n
                          | otherwise = indexofn el xs (n + 1)
                  -- where indexofn el xs n
                  --         | null xs = -1
                  --         | otherwise = if el == (head xs)
                  --                       then n
                  --                       else indexofn el (tail xs) (n+1)

  -- indexof2 el [] = -1
  -- indexof2 el (el:xs) = 0 -- doesn't work!
  -- indexof2 el (x:xs) = 1 + indexof2 el xs

  myCycle xs = xs ++ myCycle xs

  myReverse [] = []
  myReverse (x:xs) = myReverse xs ++ [x]

  fastFib _ _ 0 = 0
  fastFib _ _ 1 = 1
  fastFib _ _ 2 = 1
  fastFib n1 n2 3 = n1 + n2
  fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)

  fib :: Int -> Int
  fib n = fastFib 1 1 n
