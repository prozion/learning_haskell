import Data.Char

myElem el [] = False
myElem el xs = length (filter (== el) xs) > 0

isPalindrome xs = new_xs == reverse new_xs
  where new_xs = map toLower (filter (/= ' ') xs)

harmonic n = sum $ take n (map (\x -> 1/x) [1..])
