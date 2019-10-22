module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | total == n = Just Perfect
  | total > n = Just Abundant
  | total < n = Just Deficient
  | otherwise = Nothing
  where total = sum $ filter (\x -> rem n x == 0) [1..n-1]
