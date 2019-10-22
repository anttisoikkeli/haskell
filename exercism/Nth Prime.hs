module Prime (nth) where

nth :: Int -> Maybe Integer
nth n = if n < 1 then Nothing else Just (filter (\x -> not (any (\y -> rem x y == 0) [2 .. x - 1])) [2 ..] !! (n-1))
