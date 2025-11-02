-- | problem https://www.codewars.com/kata/530e15517bc88ac656000716/train/haskell

{-

ROT13 is a simple letter substitution cipher that replaces a letter with the letter 13 letters after it in the alphabet. ROT13 is an example of the Caesar cipher.

Create a function that takes a string and returns the string ciphered with Rot13. If there are numbers or special characters included in the string, they should be returned as they are. Only letters from the latin/english alphabet should be shifted, like in the original Rot13 "implementation".

-}

module Rot13 where
import Data.Char (ord, chr , isUpper, isLower)

alphabet :: [Char]
alphabet = ['a'..'z']


rot13 :: String -> String
rot13 = map rot13Char
  where
    rot13Char c
      | c >= 'a' && c <= 'z' = chr $ ((ord c - ord 'a' + 13) `mod` 26) + ord 'a'
      | c >= 'A' && c <= 'Z' = chr $ ((ord c - ord 'A' + 13) `mod` 26) + ord 'A'
      | otherwise = c
