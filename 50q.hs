--1
enumFromTo2 :: Int -> Int -> [Int]
enumFromTo2 a b
    | a == b = [b] 
    | otherwise = a : enumFromTo2 (a+1) b

--2
enumFromThenTo2 :: Int -> Int -> Int -> [Int]
enumFromThenTo2 a b c
    | a + b > c = []
    | otherwise = a : enumFromThenTo2 (a+b) b c

--3
concat2 :: [a] -> [a] -> [a]
concat2 [] [] = []
concat2 [] (x:xs) = x : concat2 [] xs 
concat2 (x:xs) l2 = x : concat2 xs l2

--4
getInd :: [a] -> Int -> a
getInd (x:xs) n
    | n == 0 = x
    | otherwise = getInd xs (n-1)

--5
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = (reverse2 xs) ++ [x]

--6
take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 _ [] = []
take2 n (x:xs) = x : take2 (n-1) xs

--7
drop2 :: Int -> [a] -> [a]
drop2 0 l = l
drop2 _ [] = []
drop2 n (x:xs) = drop2 (n-1) xs

--8
zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] _ = []
zip2 _ [] = []
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

--9
elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 n (x:xs)
    | n == x = True
    | otherwise = elem2 n xs

--10
replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 x n = n : replicate2 (x-1) n

--11
intersperse2 :: a -> [a] -> [a]
intersperse2 _ [x] = [x]
intersperse2 a (x:xs) = x : a : intersperse2 a xs

--12





