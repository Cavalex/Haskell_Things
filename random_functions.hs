mySort :: Ord a => [a] -> [a]
mySort [x] = [x]
mySort (x:xs)
    | x < y = x : mySort xs
    | otherwise = y : mySort (x:ys)
    where 
        (y:ys) = mySort xs

aN :: [Int] -> [(Int, Int)]
aN l = zip l [0..]
