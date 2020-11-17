import System.Environment
import Control.Monad
-- import Data.List.Split
-- import System.IO


main1 :: IO ()
main1 = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)

reverser :: IO ()
reverser = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

sampleData = ['6', '2', '\n', '2', '1', '\n']

-- myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines

main2 :: IO ()
main2 = do
  -- hGetBuffering stdin
  -- hSetBuffering stdin LineBuffering
  userInput <- getContents
  let numbers = toInts userInput
  -- print (sum numbers)
  print (sum (map (^ 2) numbers))

calc :: [String] -> Double
calc (val1:"+":val2:rest) = read val1 + read val2
calc (val1:"*":val2:rest) = read val1 * read val2

main3 :: IO ()
main3 = do
  input <- getContents
  let result = calc (lines input)
  print result

quotes = ["quote1", "quote2", "quote3", "quote4", "quote5"]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
  where quote = quotes !! (read x - 1)

main :: IO ()
main = do
  input <- getContents
  mapM_ putStrLn (lookupQuote (lines input))
