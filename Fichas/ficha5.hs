import Data.List

--1
--a
any2 :: (a -> Bool) -> [a] -> Bool
any2 _ [] = False
any2 f (x:xs) = if f x then True else any2 f xs

--b
zipWith2 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith2 _ [] _ = []
zipWith2 _ _ [] = []
zipWith2 f (x:xs) (y:ys) = (f x y):zipWith2 f xs ys

--c
takeWhile2 :: (a->Bool) -> [a] -> [a]
takeWhile2 f (x:xs) = if f x then x:takeWhile2 f xs else []

--d
dropWhile2 :: (a->Bool) -> [a] -> [a]
dropWhile2 f (x:xs) = if f x then dropWhile2 f xs else (x:xs)

--e
span2 :: (a-> Bool) -> [a] -> ([a],[a])
span2 _ [] = ([],[])
span2 f (x:xs)
    | f x = (x:rx,ry)
    | otherwise = ([],(x:xs))
    where
        (rx,ry) = span2 f xs

--f
deleteBy2 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy2 _ _ [] = []
deleteBy2 f n (x:xs) = if f n x then xs else x : deleteBy2 f n xs

--g
sortOn2 :: Ord b => (a -> b) -> [a] -> [a]
sortOn2 _ [] = []
sortOn2 _ [x] = [x]
sortOn2 f (x:xs)
    | f x < f y = x:sortOn2 f xs
    | otherwise = y:sortOn2 f (x:ys)
    where
        (y:ys) = sortOn2 f xs

--2
type Monomio = (Float, Int) --coeficiente, expoente
type Polinomio = [Monomio]

p1 :: Polinomio
p1 = [(2,3), (3,4), (5,3), (4,5)]

--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\(x,e) -> e == n) p

--b
conta :: Int ->
     Polinomio -> Int
--conta n p = length $ filter (\(x,e) -> e == n) p
conta n p = foldr (\(x,e) ac -> if e == n then ac +1 else ac ) 0 p

--c


















--3
--Matrizes, ugh
type Mat a = [[a]]

m1, m2, m3, m4 :: Mat Int

m1 = [[1,0,0], [0,1,0], [0,0,1]] --identidade

m2 = [[1,2,3], [0,4,5], [0,0,6]] --triangular superior

m3 = [[0,0,0], [0,0,0], [0,0,0]] --nula

m4 = [[1,2], [3,4], [5,6]] --3x2

--a
dimOK :: Mat a -> Bool
dimOK m = all (== tp) x 
    where 
        x = map length m
        tp = head x

--b
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m  = (length m, length (head m))

--c
-- o zipWith interior soma as linhas das matrizes, o exterior pega nas linhas das matrizes e passa para a interior
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))

--d
transposeM :: Mat a -> Mat a
transposeM = map (reverse)

--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat = undefined

--f
zipWMat :: (a -> b -> c) ->  Mat a -> Mat b -> Mat c
zipWMat = undefined
