module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)
import Data.List.Split (chunksOf)
import Data.List (transpose)

fill :: String -> Int -> String
fill word target = if length word < target then fill (word ++ " ") target else word

box :: Int -> (Int, Int)
box len = head $ filter (\(c, r) -> c * r >= len && c >= r && c - r <= 1) [(c, r) | c <- [1 .. 9], r <- [1 .. 9]]

encode :: String -> String
encode word =
  let raw = map toLower $ filter isAlphaNum word
      colRow = box (length raw)
      fixed = fill raw (fst colRow * snd colRow)
   in unwords $ transpose $ chunksOf (fst colRow) fixed
