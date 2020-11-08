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
-- The list must already be ordered for this to work
{-
group2 :: Eq a => [a] -> [[a]]
group2 [] = []
group2 [x] = [[x]]
group2 l = groupList (makeList l)

makeList :: Eq a =>  [a] -> [[a]]
makeList [] = []
makeList (x:xs) = [x] : makeList xs

groupList :: Eq a => [[a]] -> [[a]]
groupList [] = []
groupList (x:y:xs)
    | (head x == head y) && (tail y /= []) && (x /= []) = [head x, head y] : groupList ((tail y):xs)
    | (head x /= head y) && (tail y /= []) && (x /= []) = x : groupList (y:xs)
    | x == [] = groupList (y:xs)
    | otherwise = [head x, head y] : groupList ([]:xs)
groupList (x:xs) = [x]
-}

group3 :: Eq a => [a] -> [[a]]
group3 [] = []
group3 (x:xs) = (x:aux xs):aux2 xs
    where
        aux [] = []
        aux (y:ys) = if x == y then (y : aux ys) else []
        aux2 [] = []
        aux2 (z:zs) = if z == x then aux2 zs else group3 (z:zs)

--13
concat3 :: [[a]] -> [a]
concat3 [] = []
concat3 ([]:xs) = concat3 xs
concat3 ([x]:xs) = x : concat3 xs
concat3 ((x:xs):ys) = x : concat3 (xs:ys)

--14
-- Jeez this is an ugly way to do it
inits2 :: [a] -> [[a]]
inits2 l = [] : reverse (aux2 (length l) l)
    where
        aux 0 l = []
        aux n (x:xs) = x:aux (n-1) xs 
        aux2 0 l = []
        aux2 n l = (aux n l):(aux2 (n-1) l)

-- better and simpler version:
-- i tried this one first but forgot the [] on the last "l" and it wasn't working...
inits3 :: [a] -> [[a]]
inits3 [] = [[]]
inits3 l = inits3 (init l) ++ [l]

--15
tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 l = l : tails2 (tail l)

--16
-- ignore this first function
{-
isPrefixOf2 :: Eq a => [a] -> [a] -> Bool
isPrefixOf2 [] _ = True
isPrefixOf2 _ [] = False
isPrefixOf2 (x:xs) (y:ys)
    | x == y = if test (x:xs) (y:ys) == True then True else isPrefixOf2 (x:xs) ys
    | otherwise = isPrefixOf2 (x:xs) ys
    where
        test [] _ = True
        test (z:zs) (t:ts) 
            | z == t = test zs ts
            | otherwise = False
-}

isPrefixOf3 :: Eq a => [a] -> [a] -> Bool
isPrefixOf3 [] _ = True
isPrefixOf3 _ [] = False
isPrefixOf3 (x:xs) (y:ys) = x == y && isPrefixOf3 xs ys

--17
isSuffixOf2 :: Eq a => [a] -> [a] -> Bool
isSuffixOf2 [] _ = True
isSuffixOf2 _ [] = False
isSuffixOf2 l1 l2 = last l1 == last l2 && isSuffixOf2 (init l1) (init l2)

--18
isSubsequenceOf2 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf2 [] _ = True
isSubsequenceOf2 _ [] = False
isSubsequenceOf2 (x:xs) (y:ys)
    | x == y = isSubsequenceOf2 xs ys
    | otherwise = isSubsequenceOf2 (x:xs) (ys)

--19
elemIndices2 :: Eq a => a -> [a] -> [Int]
elemIndices2 _ [] = []
elemIndices2 n l = [x | x <- [0..(length l - 1)], l !! x == n]
--ou
-- simpler
elemIndices3 :: Eq a => a -> [a] -> [Int]
elemIndices3 _ [] = []
elemIndices3 n l = [i | (y,i) <- zip l [0..], y==n]









