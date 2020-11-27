{-# LANGUAGE OverloadedStrings #-}

module Main where

import Palindrome
import Data.Text as T
import Data.Text.IO as TIO

main :: IO ()
main = do
  TIO.putStrLn "Input a word"
  text <- TIO.getLine
  let response = if isPalindrome text
                 then "Yes"
                 else "No"
  TIO.putStrLn response
