import Data.Char

--1a
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

-- funA [2, 3, 5, 1]
-- 4 + funA [3, 5, 1], 4 + 9 + funA [5, 1], 4 + 9 + 25 + funA [1]
-- 4 + 9 + 25 + 1 = 39

--1b
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h : (funB t)
    else (funB t)

-- funB [8, 5, 12]
-- 8 : funB [5, 12], 8 : funB [12], 8 : 12 : funB []
-- 8 : 12 : [] = [8, 12]

--1c
funC (x:y:t) = funC t
funC [x] = []
funC [] = []

-- funC [1, 2, 3, 4, 5]
-- funC [3, 4, 5], funC [5], funC []
-- []

--1d
funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

-- funD "otrec" 
-- funD "otrec" = g [] "otrec"
-- g [] "otrec" = g 'o' "trec"
-- g ['o'] "trec" = g ['t', 'o'] "rec"
-- g ['t', 'o'] "rec" = g ['r', 't', 'o'] "ec"
-- ... = g "certo" [] = "certo"

-- O map aplica uma função a uma lista
--2a
dobros :: [Float] -> [Float]
dobros x = map (*2) x
--ou
dobros2 :: [Float] -> [Float]
dobros2 x = map (\x -> 2*x) x
--ou
dobros3 :: [Float] -> [Float]
dobros3 x = map (double) x
    where double y = y*2
--ou
dobros4 :: [Float] -> [Float]
dobros4 x = map (d) x
    where d = (*2) --yup i can do this
--ou
dobros5 :: [Float] -> [Float]
dobros5 [] = []
dobros5 (x:xs) = (2*x) : (dobros5 xs)
--list comprehension
dobros6 :: [Float] -> [Float]
dobros6 l = [x*2 | x <- l]


--2b
numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (x:xs)
    | c == x = 1 + (numOcorre c xs)
    | otherwise = (numOcorre c xs)
--list comprehension
numOcorre2 :: Char -> String -> Int
numOcorre2 c s = length [x | x <- s, x == c]


--2c
positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) = if x >= 0 then positivos xs else False

--2d
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs)
    | x >= 0 = x : soPos xs
    | otherwise = (soPos xs)
--ou
soPos2 :: [Int] -> [Int]
soPos2 l = filter (>0) l
--list comprehension
soPos3 :: [Int] -> [Int]
soPos3 l = [x | x <- l, x >= 0]


--2e
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs)
    | x < 0 = x + somaNeg xs
    | otherwise = 0 + somaNeg xs
--list comprehension
somaNeg2 :: [Int] -> Int
somaNeg2 l = sum [x | x <- l, x < 0]


--2f
tresUlt :: [a] -> [a]
tresUlt [x] = [x]
tresUlt [x,y] = [x,y]
tresUlt x = (\(x:y:z:s) -> [z,y,x]) (reverse x) 
--ou
tresUlt2 :: [a] -> [a]
tresUlt2 (a:b:c:[]) = [a,b,c]
tresUlt2 (h:t)      = tresUlt2 t


--2g
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos [(x,y)] = [y]
segundos (x:xs) = (snd x) : (segundos xs)

--2h
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a [(x,y)] = if a == x then True else False
nosPrimeiros a (x:xs)
    | a == (fst x) = True 
    | otherwise = (nosPrimeiros a xs )

--2i
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((x1,y1,z1):xs) = sT (x1,y1,z1) (sumTriplos xs)
    where sT (a1,b1,c1) (a2,b2,c2) = (a1+a2, b1+b2, c1+c2)  
--ou
sumTriplos2 :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos2 [] = (0, 0, 0)
sumTriplos2 [(a, b, c)] = (a, b, c)
sumTriplos2 ((a, b , c):(x, y, z):lista) = sumTriplos2 ((a+x, b+y, c+z):lista)


--3a
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs)
    | isDigit x = x : soDigitos xs
    | otherwise = soDigitos xs

--3b
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs)
    | isLower x = 1 + minusculas xs
    | otherwise = minusculas xs

--3c
nums :: String -> [Int]
nums [] = []
nums (x:xs)
    | isDigit x = digitToInt x : nums xs
    | otherwise = nums xs

--4
type Polinomio = [Monomio]
type Monomio = (Float, Int) -- (coeficiente, expoente)
--[(2,3),(3,4),(5,3),(4,5)] -> 2x^3 + 3x^4 + 5x^3 + 4x^5

p0 :: Polinomio
p0 = [(1,2),(3,4)]

p1 :: Polinomio
p1 = [(2,4),(1,2),(1,4)]

p2 :: Polinomio
p2 = [(2,1),(3,2),(1,3)]

p3 :: Polinomio
p3 = [(1,2),(3,4),(3,1),(4,2),(1,5),(3,3)]

--4a
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n (x:xs)
    | snd x == n = 1 + conta n xs
    | otherwise = 0 + conta n xs 

--4b
grau :: Polinomio -> Int
grau [] = 0 -- (????) probably same as having a 0, so 0^1 = 0, Also I don't think I can do 0^0
grau [(x,y)] = y
grau ((x1,y1):(x2,y2):xs)
    | y1 > y2 = grau ((x2,y1):xs)
    | otherwise = grau ((x2,y2):xs)

--4c
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n (x:xs)
    | snd x == n = x : selgrau n xs
    | otherwise = selgrau n xs

--4d
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):xs)
    | y > 0 = (x* (fromIntegral y), y-1) : deriv xs
    | otherwise = (0,0) : deriv xs -- Idk if I should put (0,1) or (0,0), but whatever
-- Probably no one will ask me the derivative of 0...

--4e
calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((x,y):xs) = x*(n^y) + calcula n xs
--ou
calcula2 :: Float -> Polinomio -> Float
calcula2 n [] = 0
calcula2 n (x:xs) = (fst x)*(n^(snd x)) + calcula2 n xs


--4f
simp :: Polinomio -> Polinomio
simp [] = []
simp (x:xs)
    | fst x == 0 = simp xs
    | otherwise = x : simp xs

--4g
mult :: Monomio -> Polinomio -> Polinomio
mult m [] = []
mult m (x:xs) = (fst m * fst x, snd m + snd x) : mult m xs

--4h
-- For this one first we need to organize it and then join the ones with common coeficients

{-
-- This one doesn't work beacuse:
-- let's say the first one is smaller then the second one,
-- then that means the second one is the second smallest in the 
-- polinomial, so that one also needs to be compared with the rest
-- of it. So to solve that problem we need to pick the 'y' from 
-- the organize funtion, exactly what we don't do in the
-- function below:

organize :: Polinomio -> Polinomio
organize [] = []
organize [x] = [x]
organize (x:y:xs)
    | snd x <= snd y = x : y : organize xs
    | otherwise = y : organize xs
-}

-- By making 'y' the first member (the smaller) in the organized
-- list, we can solve that problem:

organize :: Polinomio -> Polinomio
organize [] = []
organize [x] = [x]
organize (x:xs)
    | snd x <= snd y = x : organize xs
    | otherwise = y : organize (x:ys) -- 'x' changes places with 'y' 
    where (y:ys) = organize xs

join :: Polinomio -> Polinomio
join [] = []
join [x] = [x]
join (x:y:xs)
    | snd x == snd y = join ((fst x + fst y, snd x) : xs)
    | otherwise = x : join (y:xs) 

normaliza :: Polinomio -> Polinomio
normaliza p = join (organize p)

--4i
soma :: Polinomio -> Polinomio -> Polinomio
soma [] [] = []
soma p [] = p
soma [] p = p
soma p1 p2 = normaliza (p1 ++ p2) 
--ou
soma2:: Polinomio -> Polinomio -> Polinomio
soma2 p p1 = soma3 (normaliza p) (normaliza p1)

soma3 :: Polinomio -> Polinomio -> Polinomio
soma3 [(a,b)] [(x,y)]       = [(a+x,y)]
soma3 ((a,b):xs) ((x,y):ys) = (a+x,y) : (soma3 xs ys)  
soma3 a []                   = a
soma3 [] a                   = a  


--4j
produto :: Polinomio -> Polinomio -> Polinomio
produto [] p = []
produto p [] = []
produto (x:xs) y = (mult x y) ++ (produto xs y)

--4k
-- The organize funtion?
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena [x] = [x]
ordena (x:xs)
    | snd x <= snd y = x : ordena xs
    | otherwise = y : ordena (x:ys) -- 'x' changes places with 'y' 
    where (y:ys) = ordena xs

--4l
equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv [] p = False
equiv p [] = False
equiv p1 p2
    | (fst x == fst y) && (snd x == snd y) = equiv xs ys
    | otherwise = False
    where 
        (x:xs) = normaliza p1
        (y:ys) = normaliza p2
