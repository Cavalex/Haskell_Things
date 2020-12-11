--2
data RTree a = R a [RTree a] deriving Show


r1, r2, r3 :: RTree Int
r1 = R 42 []
r2 = R 5 [R 4 []
         , R 23 [R 44 []
               , R 55 []
               ]
         , R 33 [R 1 [R 21 []
                     ,R 67 []
                     ]
               ]
         ]
r3 = R 100 [r1,r2]

contaNR :: RTree a -> Int 
contaNR (R r []) = 1
contaNR (R r (d:ds)) = (contaNR d) + (contaNR (R r ds))

contaNR2 :: RTree a -> Int
contaNR2 (R r l) = 1 + sum (map contaNR2 l) 

somaNR :: Num a => RTree a -> a
somaNR  (R r l) = r + sum (map somaNR l)

mirrorR :: RTree a -> RTree a
mirrorR (R r l) = R r (reverse (map mirrorR l))
