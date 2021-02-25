--1
data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

a1, a2, a3, a4, a5 :: BTree Int

a1 = Node 5 (Node 4 Empty Empty) (Node 10 Empty Empty)

a2 = Node 6 (Node 5 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 8 (Node 7 Empty Empty ) (Node 9 Empty Empty))

a3 = Node 33 (Node 5 Empty
                     (Node 9 Empty Empty)) 
             (Node 10 (Node 8 Empty Empty)
                      (Node 42 (Node 33 Empty Empty) 
                               (Node 21 Empty Empty)))

a4 = Node 100 a1 a3

a5 = Node 15 a4 a2

--exercicios à parte
toListPreOrder :: BTree a -> [a]
toListPreOrder Empty = []
toListPreOrder (Node n left right) = [n] ++ (toListPreOrder left) ++ (toListPreOrder right)

toListInOrder :: BTree a -> [a]
toListInOrder Empty = []
toListInOrder (Node n left right) = (toListInOrder left) ++ [n] ++ (toListInOrder right)

toListPosOrder :: BTree a -> [a]
toListPosOrder Empty = []
toListPosOrder (Node n left right) = (toListPosOrder left) ++ [n] ++ (toListPosOrder right)


fromList :: [a] -> BTree a
fromList [] = Empty
fromList (h:t) = Node h Empty (fromList t)

type Mat a = [[a]]

m1 :: Mat Int
m1 = [[6,7,2], [1,5,9], [8,3,4]]

magic :: Mat Int -> Bool
magic mat = linhasIguaisA n mat && colunasIguaisA n mat && diagonaisIguaisA n mat
    where n = sum (head mat)

areEqual :: Int -> [Int] -> Bool
areEqual _ [] = True
areEqual n (x:xs) = if x == n then areEqual n xs else False

linhasIguaisA :: Int -> Mat Int -> Bool
linhasIguaisA n mat = areEqual n (map (sum) mat)

colunasIguaisA :: Int -> Mat Int -> Bool
colunasIguaisA n (x:xs) = if length x > 0 then colunasIguaisA n novaMatriz && (n == sum (map (head) (x:xs))) else True
    where
        novaMatriz = map (tail) (x:xs)

diagonaisIguaisA :: Int -> Mat Int -> Bool
diagonaisIguaisA n (x:xs) = if length x > 0 then diagonaisIguaisA n novaMatriz && (n == sum (map (head) (x:xs))) else True
    where
        novaMatriz = map (tail) (x:xs)


--a
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ left right) = 1 + max (altura left) (altura right) 

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ left right) = 1 + contaNodos left + contaNodos right

--c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ left right) = folhas left + folhas right

--d
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune n (Node a left right)
    | n <= 0 = Empty
    | otherwise = (Node a (prune (n-1) left) (prune (n-1) right))

--e
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (False:xs) (Node a left right) = a:path xs left
path (True:xs) (Node a left right) = a:path xs right

--f

--g

--h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x,y,z) l r) = ((Node x xl xr),(Node y yl yr),(Node z zl zr))
    where 
        (xl,yl,zl) = unzipBT l
        (xr,yr,zr) = unzipBT r

--2

--3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String 
data Regime = ORD | TE | MEL  deriving Show 
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
      deriving Show
type Turma = BTree Aluno  -- arvore binaria de procura (ordenada por numero)

al1,al2,al3,al4 :: Aluno
al1 = (1234, "Xico", TE, Aprov 12)
al2 = (2345, "Manuel", ORD, Aprov 16)
al3 = (3456, "Ana", MEL, Faltou)
al4 = (4567, "Carlos", TE, Aprov 20)
 
t1 :: Turma
t1 = Node al2 (Node al1 Empty Empty) (Node al3 Empty Empty)

--a
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False 
inscNum n (Node (nu,no,re,cl) e d)
        | n == nu = True
        | n < nu = (inscNum n e)
        | n > nu = (inscNum n d)

--b
inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (nu,no,re,cl) e d)
         | n == no = True
         | otherwise = (inscNome n e) || (inscNome n d)   


acrescentaAluno :: Aluno -> Turma -> Turma 
acrescentaAluno a Empty = Node a Empty Empty
acrescentaAluno (nu1,no1,re1,cl1) (Node (nu,no,re,cl) e d) 
                | nu1 == nu = (Node (nu,no,re,cl) e d)
                | nu1 < nu = (Node (nu,no,re,cl) (acrescentaAluno (nu1,no1,re1,cl1) e) d)
                | nu1 > nu = (Node (nu,no,re,cl) e (acrescentaAluno (nu1,no1,re1,cl1) d)) 


minimo :: Turma -> Aluno 
minimo (Node r Empty d) = r
minimo (Node r e d) = minimo e

sMin :: Turma -> Turma 
sMin (Node r Empty d) = d 
sMin (Node r e d) = Node r (sMin e) d

minSmin :: BTree a -> (a,BTree a)
minSmin (Node r Empty d) = (r,d)
minSmin (Node r e d) = (x,Node r y d)
    where (x,y) = minSmin e

removeAluno :: Aluno -> Turma -> Turma 
removeAluno _ Empty = Empty
removeAluno al@(nuA,_,_,_) t@(Node r@(nuR,_,_,_) e d)
      | nuA == nuR = removeRaiz t
      | nuA < nuR = Node r (removeAluno al e) d
      | nuA > nuR = Node r e (removeAluno al d)
   where removeRaiz (Node x Empty d) = d
         removeRaiz (Node x e Empty) = e
         removeRaiz (Node r e d) = Node md e d' 
            where (md,d') = minSmin d