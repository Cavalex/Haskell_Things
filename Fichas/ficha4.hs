import Data.Char

--1
--a [6,12,18]
--b [6,12,18]
--c [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
--d [1,1,4,4,9,9,16,16,25,25]

--2
da = [ 2^x | x <- [0..10]]
db = [ (x,y) | x <- [1..5], y <- [1..5], x + y == 6]
dc = [ [1..x] | x <- [1..5]]
-- aqui podia tbm usar o replicate, tipo "replicate x 1"
dd = [ take x (repeat 1) | x <- [1..5]]
de = [ factorial x | x <- [1..6]]
    where 
        factorial 0 = 1
        factorial x = x * factorial (x - 1)
-- à frente do product está uma lista que vai de 1 a x,
-- e o produto disso é basicamente o fatorial de x
de2 = [ product [y | y <- [1..x]] | x <- [1..6]]

--3
digitAlpha :: String -> (String, String)
digitAlpha [] = ([],[])
digitAlpha (x:xs)
    | isDigit x = (x:sd, sa)
    | otherwise = (sd, x:sa)
    where
        (sd, sa) = digitAlpha xs


digitAlpha2 :: String -> (String,String)
digitAlpha2 string = 
    foldl (\(alpha,digit) x -> if isDigit x then (alpha,digit ++ [x]) else if isAlpha x then (alpha ++ [x],digit) else (alpha,digit)) ("","") string

--4
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs)
    | x < 0 = (1+sn,sz,sp)
    | x == 0 = (sn,1+sz,sp)
    | otherwise = (sn,sz,1+sp)
    where
        (sn,sz,sp) = nzp xs

--5
divMod2 :: Integral a => a -> a -> (a, a)
divMod2 n d 
    | n < d = (0,n)
    | otherwise = (1+n2, r)
    where (n2,r) = divMod2 (n-d) d

--6
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits l = aux l (length l - 1)
    where
        aux [] _ = 0
        aux (x:xs) c = x*(10^c) + (aux xs (c-1))

-- esta não usa o length, mas não é tão intuitiva
fromDigits2 :: [Int] -> Int
fromDigits2 [] = 0
fromDigits2 (x:xs) = aux xs x
    where
        aux [] c = c
        aux (y:ys) c = aux ys (c*10 + y)

--7
-- ??? nao funciona
--maxSumInit :: (Num a, Ord a) => [a] -> a
--maxSumInit l = maximum [ m | m <- init l]

--8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

