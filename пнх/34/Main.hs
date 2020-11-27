module Main where

import qualified Palindrome
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  putStrLn "Input a word"
  text <- TIO.getLine
  let response = if Palindrome.isPalindrome text
                 then "Yes"
                 else "No"
  putStrLn response
