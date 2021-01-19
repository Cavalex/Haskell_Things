--1
--a
intersect2 :: Eq a => [a] -> [a] -> [a]
intersect2 [] _ = []
intersect2 (x:xs) l
    | x `elem` l = x:intersect2 xs l
    | otherwise = intersect2 xs l

--b
tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 (x:xs) = (x:xs):tails2 xs

--2
type ConjInt = [Intervalo]
type Intervalo = (Int, Int)

ci1 :: ConjInt
ci1 = [(1,4),(7,8),(19,19),(21,23)]

n1 :: [Int]
n1 = [1,2,3,4,7,8,19,21,22,23]

--a
elems :: ConjInt -> [Int]
elems [] = []
elems ((x,n):xs) = if x == n then x:elems(xs) else x:elems((x+1,n):xs)

--b
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj (x:xs) = aux xs x x
    where
        aux [] elem num = [(elem, num)]
        aux (y:ys) elem num
            | y - num == 1 = aux ys elem (y)
            | otherwise = (elem, num):aux ys y y

--3
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String
    deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]

c1 :: Contacto
c1 = Casa 111

c2 :: Contacto
c2 = Trab 222

cs1 :: [Contacto]
cs1 = [c1, c2, Tlm 911, Email "mat"]

cs2 :: [Contacto]
cs2 = [Casa 333, Trab 444, Tlm 966, Email "abc"]

cs3 :: [Contacto]
cs3 = [Casa 333, Trab 444, Tlm 966]

cs4 :: [Contacto]
cs4 = [Casa 333, Trab 444, Tlm 966, Tlm 253]

agenda1 :: Agenda
agenda1 = [("Mateus", cs1), ("NomeTeste", cs2), ("NomeTeste2", cs2)]

agenda2 :: Agenda
agenda2 = [("Mateus", (Email "a@a.com" ): cs1), ("NomeTeste", cs2), ("NomeTeste2", cs3)]

--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((n,y):xs)
    | n == nome = ((n,(Email email):y):xs)
    | otherwise = (n,y):acrescEmail nome email xs

--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails n ((x,y):xs)
    | n == x = Just (getEmails y)
    | otherwise = verEmails n xs
    where
        getEmails [] = []
        getEmails ((Email e):es) = e:getEmails es
        getEmails (_:es) = getEmails es
        
--c
consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta ((Casa n):xs) = (n:csxe, csxd)
    where 
        csxe = fst (consulta xs)
        csxd = snd (consulta xs)
consulta ((Tlm n):xs) = (n:csxe, csxd)
    where 
        csxe = fst (consulta xs)
        csxd = snd (consulta xs)
consulta ((Email e):xs) = (csxe, e:csxd)
    where 
        csxe = fst (consulta xs)
        csxd = snd (consulta xs)
consulta (_:xs) = (csxe, csxd)
    where 
        csxe = fst (consulta xs)
        csxd = snd (consulta xs)

--d
consultaIO :: Agenda -> IO()
consultaIO agenda = do 
    c <- getLine
    print (look c agenda)

look :: String -> Agenda -> [Contacto]
look _ [] = []
look n ((a,b):xs)
    | n == a = b
    | otherwise = look n xs

--4
data RTree a = R a [RTree a] deriving (Show, Eq)

--a
paths :: RTree a -> [[a]]


