{-# LANGUAGE OverloadedStrings #-}

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello, " <> name <> "!"

main :: IO ()
main = do
  TIO.putStrLn "Hello! What is your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main2 :: IO ()
main2 = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  TIO.putStrLn $ (T.pack . show . sum) numbers
