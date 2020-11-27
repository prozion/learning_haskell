-- import Control.Monad

ends = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- genYearDays :: Int -> [Int]
-- genYearDays month = [ days | days <- [1 .. (monthDays !! (month - 1))]]

dates :: [Int]
dates = [ date | end <- ends, date <- [1..end]]


dates2 :: [Int]
dates2 = do
  end <- ends
  date <- [1..end]
  return date

dates3 :: [Int]
dates3 = ends >>=
          (\ end ->
            [1..end] >>=
              (\ date -> return date))
