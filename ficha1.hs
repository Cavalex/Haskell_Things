import Data.Char (ord, chr)  

-- 1
perimetro1 :: Double -> Double
perimetro1 r = pi * r

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
    | raiz > 0  = 2
    | raiz < 0  = 0 
    | otherwise = 1
    where raiz = b^2 - 4*a*c

raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    | nRaizes a b c == 2 = [raizPos / div, raizNeg / div]
    | nRaizes a b c == 1 = [raizPos / div]
    | otherwise = []
    where
        raiz = b^2 - 4*a*c
        raizPos = (-b + raiz)
        raizNeg = (-b - raiz)
        div = 2 * a

type Hora = (Int, Int)

--3a
isValidHour :: Hora -> Bool
isValidHour (a,b)
    | (a >= 0 && a < 24) && (b >= 0 && b < 60) = True
    | otherwise = False

--3b
isBiggerThan :: Hora -> Hora -> Bool
isBiggerThan (a, b) (c, d)
    | (a > c) || (a == c && b > d) = True
    | otherwise = False

--3c
toMinutes :: Hora -> Int
toMinutes (a, b) = a*60 + b

--3d
toHours :: Int -> Hora
toHours a = (div a 60, mod a 60)

--3e
hourDif :: Hora -> Hora -> Int
hourDif (a, b) (c, d) = toMinutes (a, b) - toMinutes (c, d)

--3f
addMinutes :: Hora -> Int -> Hora
addMinutes (a, b) c = toHours (toMinutes (a, b) + c) 

--4
data Hora2 = H Int Int
    deriving (Show, Eq)

--4a
isValidHour2 :: Hora2 -> Bool
isValidHour2 (H a b)
    | (a >= 0 && a < 24) && (b >= 0 && b < 60) = True
    | otherwise = False

--4b
isBiggerThan2 :: Hora2 -> Hora2 -> Bool
isBiggerThan2 (H a b) (H c d)
    | (a > c) || (a == c && b > d) = True
    | otherwise = False

--4c
toMinutes2 :: Hora2 -> Int
toMinutes2 (H a b) = a*60 + b

--4d
toHours2 :: Int -> Hora2
toHours2 a = (H (div a 60) (mod a 60))

--4e
hourDif2 :: Hora2 -> Hora2 -> Int
hourDif2 (H a b) (H c d) = toMinutes2 (H a b) - toMinutes2 (H c d)
--hourDif2 h1 h2 = toMinutes2 h1 - toMinutes2 h2

--4f
addMinutes2 :: Hora2 -> Int -> Hora2
addMinutes2 h1 a = toHours2 ((toMinutes2 h1) + a)

--5
data Semaforo = Verde | Amarelo | Vermelho
    deriving (Show, Eq)

--5a
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next x = Verde

--5b
stop :: Semaforo -> Bool
stop Verde = False
stop x = True
--stop _ = True

--5c
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

--6
data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show, Eq)

--6a
posx :: Ponto -> Double
posx (Cartesiano x y) = y
posx (Polar r a) = r * (cos a)

--6b
posy :: Ponto -> Double
posy (Cartesiano x y) = x
posy (Polar r a) = r * (sin a)

--6c
raio:: Ponto -> Double
raio (Cartesiano x y) = sqrt ((x+0)^2 + (y+0)^2)
raio (Polar r a) = r

--6d
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y / x)
angulo (Polar r a) = a

--6e
polarToCart :: Ponto -> Ponto
polarToCart (Polar r a) = (Cartesiano (r*(cos a)) (r*(sin a)))

distP :: Ponto -> Ponto -> Double
distP (Polar r1 a1) (Polar r2 a2) = 
    distP (polarToCart (Polar r1 a1)) (polarToCart (Polar r2 a2))
distP (Polar r a) p =
    distP (polarToCart (Polar r a)) p
distP p (Polar r a) =
    distP p (polarToCart (Polar r a))
distP (Cartesiano x1 y1) (Cartesiano x2 y2) = 
    dist (x1, y1) (x2, y2)

--7
data Figura = Circulo Ponto Double
    | Rectangulo Ponto Ponto
    | Triangulo Ponto Ponto Ponto
    deriving (Show, Eq)

--7a
poligono :: Figura -> Bool
poligono (Rectangulo p1 p2) = True
poligono (Triangulo p1 p2 p3) = True
poligono x = False

--7b
vertices :: Figura -> [Ponto]
vertices (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = 
    [(Cartesiano x1 0), (Cartesiano x2 0), (Cartesiano x1 y1), (Cartesiano x2 y2)]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]
vertices _ = []

--7c
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distP p1 p2
        b = distP p2 p3
        c = distP p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) --formula de Heron
area (Circulo p r) = pi * r^2
area (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = 
    let altura = dist (x1, 0) (x1, y1)
        base = dist (x1, 0) (y1, 0)
    in base * altura
area (Rectangulo (Cartesiano x1 y1) (Polar r a)) =
    let p2 = polarToCart (Polar r a)
    in area (Rectangulo (Cartesiano x1 y1) p2)
area (Rectangulo (Polar r a) (Cartesiano x1 y1)) = 
    let p1 = polarToCart (Polar r a)
    in area (Rectangulo p1 (Cartesiano x1 y1))
area (Rectangulo (Polar r1 a1) (Polar r2 a2)) = 
    let p1 = polarToCart (Polar r1 a1)
        p2 = polarToCart (Polar r2 a2)
    in area (Rectangulo p1 p2)

--7d
perimetro :: Figura -> Double
perimetro (Circulo p r) = 2 * pi * r
perimetro (Triangulo p1 p2 p3) =
    let s1 = distP p1 p2
        s2 = distP p2 p3
        s3 = distP p3 p1
    in s1 + s2 + s3
perimetro (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = 
    let altura = dist (x1, 0) (x1, y1)
        base = dist (x1, 0) (y1, 0)
    in altura * 2 + base * 2
perimetro (Rectangulo (Cartesiano x1 y1) (Polar r a)) =
    let p2 = polarToCart (Polar r a)
    in perimetro (Rectangulo (Cartesiano x1 y1) p2)
perimetro (Rectangulo (Polar r a) (Cartesiano x1 y1)) = 
    let p1 = polarToCart (Polar r a)
    in perimetro (Rectangulo p1 (Cartesiano x1 y1))
perimetro (Rectangulo (Polar r1 a1) (Polar r2 a2)) = 
    let p1 = polarToCart (Polar r1 a1)
        p2 = polarToCart (Polar r2 a2)
    in perimetro (Rectangulo p1 p2)

--8a
isLower :: Char -> Bool
isLower c = ord c >= 97 && ord c <= 122

--8b
isDigit :: Char -> Bool
isDigit c = ord c >= 48 && ord c <= 57

--8c
isAlpha :: Char -> Bool
isAlpha c = isLower c || (ord c >= 65 && ord c <= 90)

--8d
toUpper :: Char -> Char
toUpper c = chr (ord c - 32)

--8e
intToDigit :: Int -> Char
intToDigit n = chr (n + 48)

--8f
digitToInt :: Char -> Int
digitToInt c = ord c - 48


