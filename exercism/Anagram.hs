module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\x -> map toLower x /= map toLower xs && sort (map toLower x) == sort (map toLower xs))
