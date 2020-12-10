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
path = undefined

--f

--g

--h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x,y,z) l r) = ((Node x xl xr),(Node y yl yr),(Node z zl zr))
    where 
        (xl,yl,zl) = unzipBT l
        (xr,yr,zr) = unzipBT r

