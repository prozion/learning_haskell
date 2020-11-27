module Palindrome (isPalindrome, preprocess) where

import Data.Char (toLower, isSpace, isPunctuation)
import qualified Data.Text as T

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.pack $ filter (not . isSpace) (T.unpack text)

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.pack $ filter (not . isPunctuation) (T.unpack text)

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.pack $ map toLower (T.unpack text)

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = (T.reverse cleanText) == cleanText
  where cleanText = preprocess text
