module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.List.Split (chunksOf)

countAround :: [String] -> Int -> Int -> Int -> Int -> Int
countAround board row col width height =
  let
    cols = [(max 0 (col-1))..(min (width-1) (col+1))]
    rows = [(max 0 (row-1))..(min (height-1) (row+1))]
    values = map (\(i,j) -> board !! i !! j) [(i,j) | i <- rows, j <- cols]
  in sum (map (\x -> if x == '*' then 1 else 0) values)

stringify :: (Int, Char) -> Char
stringify (i, c)
  | c == '*' = '*'
  | i == 0 = ' '
  | otherwise = intToDigit i

annotate :: [String] -> [String]
annotate board =
  let width = length $ head board
      height = length board
      numbered =
        map (\(i, j) -> countAround board i j width height) [(i, j) | i <- [0 .. (height - 1)], j <- [0 .. (width - 1)]]
      tupled = zip numbered (concat board)
      stringed = map stringify tupled
   in if null board || (width == 0)
         then board
         else chunksOf width stringed
