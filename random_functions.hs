mySort :: Ord a => [a] -> [a]
mySort [x] = [x]
mySort (x:xs)
    | x < y = x : mySort xs
    | otherwise = y : mySort (x:ys)
    where 
        (y:ys) = mySort xs