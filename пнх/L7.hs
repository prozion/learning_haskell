mytake n xs
  | n == 0 = []
  | null xs = []
mytake n (x:xs) = x:(mytake (n-1) xs)

myLast (x:[]) = x
myLast (_:xs) = myLast xs

myTail [] = []
myTail (_:xs) = xs

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)
