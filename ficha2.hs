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


--2b
{-
numOcorre :: Char -> String -> Int
numOcorre c s 
    | c == 
-}

--2c
positivos :: [Int] -> Bool
positivos [] = False
positivos [x] = if x>=0 then True else False
positivos (x:xs) = if x >= 0 then positivos xs else False

--2d
soPos :: [Int] -> [Int]
soPos [x] = if x >= 0 then [x] else []
soPos (x:xs)
    | x >= 0 = x : soPos xs
    | otherwise = (soPos xs)
