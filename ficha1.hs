-- 1

perimetro :: Double -> Double
perimetro r = pi * r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (a, b) (c, d) = sqrt ((a-c)^2 +(b-d)^2)

primUlt :: [a] -> (a, a)
primUlt a = (head a, last a)

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

truncaImpar :: [a] -> [a]
truncaImpar a = if ((length a) `mod` 2 /= 0) then tail a else a

max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 a (max2 b c)

{-
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c
    | b^2 > (4.0*a*c)      = 2
    | b^2 == (4.0*a*c)     = 1
    | b^2 < (4.0*a*c)      = 0
    -- | otherwise            = 4 Só para testar o otherwise
-}

-- OU:
-- Mais elegante se usar o where no final, posso testar com let no início tbm

nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c
--  | Condiçao = Resultado de True
    | raiz > 0  = 2
    | raiz < 0  = 0 
    | otherwise = 1
    where raiz = b^2 - 4*a*c

raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    | nRaizes a b c == 2 = [(-b + raiz) / 2 * a, (-b - raiz) / 2 * a]
    | nRaizes a b c == 1 = [(-b + raiz) / 2 * a]
    | otherwise = []
    where raiz = b^2 - 4*a*c

