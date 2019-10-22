module Luhn (isValid) where

first :: [Int] -> [Int]
first [] = []
first (x:xs) = x:second xs

second :: [Int] -> [Int]
second [] = []
second (x:xs) = first xs

doubler :: Int -> Int
doubler x = if x == 9 then 9 else rem (2 * x) 9

isValid :: String -> Bool
isValid text =
  let
    numbersReversed = reverse $ map (read . (:"")) $ filter (/=' ') text :: [Int]
    firsts = first numbersReversed
    seconds = map doubler $ second numbersReversed
    total = sum firsts + sum seconds
  in rem total 10 == 0 && length numbersReversed >= 2
