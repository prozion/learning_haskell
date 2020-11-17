
module L4 where

import Data.List

counter = (\ x ->
                (\ x ->
                  (\ x -> x)
                  x + 1)
                x + 1)

compareLastNames name1 name2 =
  if lastName1 > lastName2
    then GT
    else
      if lastName1 < lastName2
      then LT
      else
        if firstName1 > firstName2
          then GT
          else
            if firstName1 < firstName2
              then LT
              else EQ
  where
    (firstName1, lastName1) = name1
    (firstName2, lastName2) = name2

test_list = [("Ian", "Curtis"), ("Stephen", "Morris"), ("Bernard", "Samner"), ("Peter", "Hook"), ("Jack", "Morris")]

-- test_f = sortBy compareLastNames
--
-- -- test_f test_list

inc n = n + 1
double n = n * 2
square n = n^2
