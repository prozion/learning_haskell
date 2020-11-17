-- module Main where
--
-- import "L4"

inc n = n + 1
double n = n * 2
square n = n^2

ifEven f x =
  if even x
  then f x
  else x

genIfEven f = (\x -> ifEven f x)

-- ifEvenInc = genIfEven inc
-- ifEvenDouble = genIfEven double
-- ifEvenSquare = genIfEven square

ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

binaryPartialApplication f x = (\y -> f x y)
