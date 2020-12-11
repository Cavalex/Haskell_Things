--1
enumFromTo2 ::  Int -> Int -> [Int]
enumFromTo2 a b = if a /= b then a : enumFromTo2 (a+1) b else [a]

--2
--VER
enumFromThenTo2 ::  Int -> Int-> Int -> [Int]
enumFromThenTo2 a b c
    | b > c && b > a = [a]
    | b < c && a > b = [a]
    | otherwise = a : enumFromThenTo2 b (b-a + b) c

--3
concat2 :: [a] -> [a] -> [a]
concat2 l1 [] = l1
concat2 l1 (x:xs) = concat2 (l1 ++ [x]) xs

--4
getInd :: [a] -> Int -> a
getInd [] _ = error "Indice not in list!"
getInd (x:xs) 0 = x
getInd (x:xs) n = getInd xs (n-1)

--5
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = (reverse2 xs) ++ [x]

--6
take2 :: Int -> [a] -> [a]
take2 _ [] = []
take2 0 _ = []
take2 n (x:xs) = x:take2 (n-1) xs

--7
drop2 :: Int -> [a] -> [a]
drop2 _ [] = []
drop2 0 l = l
drop2 n (x:xs) = drop2 (n-1) xs  

--8
zip2 :: [a] -> [b] -> [(a,b)]
zip2 _ [] = []
zip2 [] _ = []
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

--9
elem2 :: Eq a => a -> [a] -> Bool
elem2 n [] = False
elem2 n (x:xs) = if n == x then True else elem2 n xs

--10
replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x:replicate2 (n-1) x 

--11
--VER
intersperse2 :: a -> [a] -> [a]
intersperse2 _ [] = []
intersperse2 _ [x] = [x]
intersperse2 n (x:xs) = x:n:intersperse2 n xs

--12
--VER
group2 :: Eq a => [a] -> [[a]]
group2 [] = []
group2 (x:xs) = (x:aux xs):aux2 xs
    where
        aux [] = []
        aux (y:ys) = if x == y then (y:aux ys) else []
        aux2 [] = []
        aux2 (z:zs) = if z == x then aux2 zs else group2 (z:zs)

--13
--VER
concat3 :: [[a]] -> [a]
concat3 [] = []
concat3 ([]:ys) = concat3 ys
concat3 ((x:xs):ys) = x:concat3 ((xs):ys)

--14
--VER
inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 l = inits2 (init l) ++ [l]

--15
--VER
tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 l = l : tails2 (tail l)

--16
isPrefixOf2 :: Eq a => [a] -> [a] -> Bool
isPrefixOf2 [] _ = True
isPrefixOf2 _ [] = False
isPrefixOf2 (x:xs) (y:ys) = x == y && isPrefixOf2 xs ys

--17
isSuffixOf2 :: Eq a => [a] -> [a] -> Bool
isSuffixOf2 [] _ = True
isSuffixOf2 _ [] = False
isSuffixOf2 l1 l2 = last l1 == last l2 && isSuffixOf2 (init l1) (init l2)

--18
--VER
isSubsequenceOf2 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf2 l [] = False
isSubsequenceOf2 [] _ = True
isSubsequenceOf2 (x:xs) (y:ys)
    | x == y = isSubsequenceOf2 xs ys
    | otherwise = isSubsequenceOf2 (x:xs) ys

--19
--VER
elemIndices2 :: Eq a => a -> [a] -> [Int]
elemIndices2 _ [] = []
elemIndices2 n l = [i | (x,i) <- zip l [0..], x == n]

--20
--VER
nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x:xs) = x: nub2 (filter (/=x) xs)

--21
delete2 :: Eq a => a -> [a] -> [a]
delete2 _ [] = []
delete2 n (x:xs) = if x == n then xs else x:delete2 n xs

delete'' :: Eq a => a -> [a] -> [a]
delete'' = filter . (/=)

--22
--VER
-- para esta é necessária a funçao delete
remove2 :: Eq a => [a] -> [a] -> [a]
remove2 l [] = l
remove2 [] _ = []
remove2 l (x:xs) = remove2 (delete2 x l) xs 

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) [] _ = []
(\\) l (x:xs) = (\\) (delete2 x l) xs 

--23
union2 :: Eq a => [a] -> [a] -> [a]
union2 [] l = l
union2 l [] = l
union2 l1 (x:xs)
    | x `elem` l1 = union2 l1 xs
    | otherwise = union2 (l1 ++ [x]) xs

--24
--VER
intersect2 :: Eq a => [a] -> [a] -> [a]
intersect2 [] l = []
intersect2 (x:xs) l
    | x `elem` l = x:intersect2 xs l
    | otherwise = intersect2 xs l

--25
--VER
insert2 :: Ord a => a -> [a] -> [a]
insert2 n [] = [n]
insert2 n (x:xs) = if n > x then x:insert2 n xs else n:x:xs 

--26
--VER
unwords2 :: [String] -> String
unwords2 [] = []
unwords2 [x] = x
unwords2 (x:xs) = x ++ " " ++ unwords2 xs

--27
unlines2 :: [String] -> String
unlines2 [] = []
unlines2 (x:xs) = x ++ "\n" ++ unlines2 xs

--28
--VER
pMaior2 :: Ord a => [a] -> Int
pMaior2 [] = error "Erro, Lista Vazia."
pMaior2 (x:xs) = aux 0 0 x xs
    where
        aux _ iMaior _ [] = iMaior
        aux iAtual iMaior maior (y:ys)
            | y > maior = aux (iAtual+1) (iAtual+1) y ys
            | otherwise = aux (iAtual+1) iMaior maior ys

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:xs)
    | x `elem` xs = True
    | otherwise = temRepetidos xs

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs)
    | x `elem` ['0'..'9'] = x : algarismos xs
    | otherwise = algarismos xs

--31
--VER
posImpares :: [a] -> [a]
posImpares [] = []
posImpares l = aux 0 l
    where
        aux _ [] = []
        aux n (y:ys)
            | odd n = y : aux (n+1) ys
            | otherwise = aux (n+1) ys

--32
posPares :: [a] -> [a]
posPares [] = []
posPares l = aux 0 l
    where
        aux _ [] = []
        aux n (x:xs)
            | even n = x : aux (n+1) xs
            | otherwise = aux (n+1) xs

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if y >= x then True && isSorted (y:xs) else False 

--34
--VER
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert2 x (iSort xs)

--35
--VER
menor :: String -> String -> Bool
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys)
    | x < y = True
    | x > y = False
    | otherwise = menor xs ys

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet c ((a,i):xs) = if c == a then True else elemMSet c xs

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,i):xs) = i + lengthMSet xs

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,i):xs)
    | i <= 0 = converteMSet xs
    | otherwise = a:converteMSet ((a,(i-1)):xs)

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,i):xs)
    | x == a = ((a,(i+1)):xs)
    | otherwise = (a,i):insereMSet x xs

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = error "Lista vazia, não há nada para remover!"
removeMSet x ((a,i):xs)
    | x == a && i > 1 = ((a,(i-1)):xs)
    | x == a && i == 1 = (xs)
    | otherwise = (a,i):removeMSet x xs 

--41
--VER
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = aux xs x 1
    where
        aux [] elem num = [(elem, num)]
        aux (y:ys) elem num
            | y == elem = aux ys elem (num+1)
            | otherwise = (elem, num):aux ys y 1

--42
--VER
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):xs) = (a:rx, ry)
    where (rx,ry) = partitionEithers xs
partitionEithers ((Right a):xs) = (rx, a:ry)
    where (rx,ry) = partitionEithers xs

--43
--VER
--ATENCAO ÀS SETAS NO CASE!!
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
    case x of Just a -> a:catMaybes xs
              Nothing -> catMaybes xs

--44
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (m:ms) =
    case m of Norte -> posicao (x,y+1) ms
              Sul -> posicao (x,y-1) ms
              Este -> posicao (x+1,y) ms
              Oeste -> posicao (x-1,y) ms

--45
caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xf > xi = Este : caminho (xi+1,yi) (xf, yf)
    | xf < xi = Oeste : caminho (xi-1,yi) (xf,yf)
    | yf > yi = Norte : caminho (xi,yi+1) (xf,yf)
    | yf < yi = Sul : caminho (xi,yi-1) (xf,yf)
    | otherwise = []

--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (x:xs) = 
    case x of Norte -> True && vertical xs
              Sul -> True && vertical xs
              _ -> False

--47
--VER
--POSSO OMITIR A SQRT DA EQUAÇAO DE COMPARAÇAO
data Posicao = Pos Int Int
    deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral ((Pos x1 y1):(Pos x2 y2):xs) = 
    if ((x1-0)^2 + (y1-0)^2) < ((x2-0)^2 + (y2-0)^2)
        then maisCentral ((Pos x1 y1):xs)
        else maisCentral ((Pos x2 y2):xs)

--48
--VER
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x1 y1) l = 
    filter (\(Pos x2 y2) -> abs (x1 - x2) + abs (y1 - y2) == 1) l

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos _ y1):(Pos x y2):xs)
    | y1 == y2 = True && mesmaOrdenada ((Pos x y2):xs)
    | otherwise = False

--50
--VER?
data Semaforo = Verde | Amarelo | Vermelho
    deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = if contaNaoVermelho l > 1 then False else True
    where
        contaNaoVermelho [] = 0
        contaNaoVermelho (x:xs) =
            case x of Vermelho -> 0 + contaNaoVermelho xs
                      _ -> 1 + contaNaoVermelho xs 




