--1
data Hora = H Int Int
    deriving Show

type Etapa = (Hora, Hora)
type Viagem = [Etapa]

isValidHour :: Hora -> Bool
isValidHour (H a b)
    | (a >= 0 && a < 24) && (b >= 0 && b < 60) = True
    | otherwise = False

isBiggerThan :: Hora -> Hora -> Bool
isBiggerThan (H a b) (H c d)
    | (a > c) || (a == c && b > d) = True
    | otherwise = False

toMinutes :: Hora -> Int
toMinutes (H a b) = a*60 + b

toHours :: Int -> Hora
toHours a = (H (div a 60) (mod a 60))

hourDif :: Hora -> Hora -> Int
hourDif (H a b) (H c d) = toMinutes (H a b) - toMinutes (H c d)

addMinutes :: Hora -> Int -> Hora
addMinutes h1 a = toHours ((toMinutes h1) + a)

-- Viagem exemplo: [(H 9 39, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

--a
isValidStage :: Etapa -> Bool
isValidStage (H x1 y1, H x2 y2) = 
    isValidHour (H x1 y1) 
    && isValidHour (H x2 y2)
    && isBiggerThan (H x2 y2) (H x1 y1)
--b
isValidTrip :: Viagem -> Bool
isValidTrip [] = True
isValidTrip (x:y:xs)
    | isValidStage x && isValidStage y && (isBiggerThan (fst y) (snd x)) = True && isValidTrip (y:xs)
    | otherwise = False
isValidTrip (x:xs) = True

--c
getDepartureHour :: Viagem -> Hora
getDepartureHour v = fst (head v)

getArrivalHour :: Viagem -> Hora
getArrivalHour v = snd (last v)

getArrivalAndDepartureHour :: Viagem -> (Hora, Hora)
getArrivalAndDepartureHour v = (getDepartureHour v, getArrivalHour v)

--d
getTripTime :: Viagem -> Int
getTripTime [] = 0
getTripTime ((x,y):xs) 
    | isValidStage (x,y) = hourDif y x + getTripTime xs
    | otherwise = error "A stage isn't valid!"

--e
getWaitingTime :: Viagem -> Int
getWaitingTime [] = 0
getWaitingTime ((x1,y1):(x2,y2):xs)
    | isValidStage (x1,y1) && isValidStage (x2,y2) = 
        getTimeBtStages (x1,y1) (x2,y2)
        + getWaitingTime ((x2,y2):xs)
    | otherwise = error "A stage isn't valid!"
    where 
        getTimeBtStages (a1, b1) (a2, b2) = hourDif a2 b1
getWaitingTime (x:xs) = 0

--f
getTotalTime :: Viagem -> Int
getTotalTime v = getTripTime v + getWaitingTime v

--2
data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show, Eq)

type Poligonal = [Ponto]

p1 :: Poligonal
p1 = [Cartesiano 2 1, Polar (sqrt 2) (pi/4)]

p2 :: Poligonal
p2 = [Cartesiano 2 1, Polar (sqrt 2) (pi/4), Cartesiano 2 1]

p3 :: Poligonal
p3 = [Cartesiano 2 1, Polar (sqrt 2) (pi/4), Cartesiano 4 0, Cartesiano 2 1]

posx :: Ponto -> Double
posx (Cartesiano x y) = y
posx (Polar r a) = r * (cos a)

posy :: Ponto -> Double
posy (Cartesiano x y) = x
posy (Polar r a) = r * (sin a)

raio:: Ponto -> Double
raio (Cartesiano x y) = sqrt ((x+0)^2 + (y+0)^2)
raio (Polar r a) = r

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y / x)
angulo (Polar r a) = a

polarToCart :: Ponto -> Ponto
polarToCart (Polar r a) = (Cartesiano (r*(cos a)) (r*(sin a)))

dist :: (Double, Double) -> (Double, Double) -> Double
dist (a, b) (c, d) = sqrt ((a-c)^2 +(b-d)^2)

distP :: Ponto -> Ponto -> Double
distP (Polar r1 a1) (Polar r2 a2) = 
    distP (polarToCart (Polar r1 a1)) (polarToCart (Polar r2 a2))
distP (Polar r a) p =
    distP (polarToCart (Polar r a)) p
distP p (Polar r a) =
    distP p (polarToCart (Polar r a))
distP (Cartesiano x1 y1) (Cartesiano x2 y2) = 
    dist (x1, y1) (x2, y2)

--a
getLength :: Poligonal -> Double
getLength [] = 0
getLength [x] = 0
getLength (x:y:xs) = distP x y + getLength (y:xs)

--b
-- I'm dumb
isClosed :: Poligonal -> Bool
isClosed [x] = False
isClosed [x,y] = False
isClosed p = same (head p) (last p)
    where 
        same (Polar x1 y1) p2 =
            same (polarToCart (Polar x1 y1)) p2
        same (Cartesiano x1 y1) (Polar x2 y2) =
            same (Cartesiano x1 y1) (polarToCart (Polar x2 y2))
        same (Cartesiano x1 y1) (Cartesiano x2 y2) =
            x1 == x2 && y1 == y2

--or 
isClosed2 :: Poligonal -> Bool
isClosed2 [x] = False
isClosed2 p = head p == last p

--c
data Figura = Circulo Ponto Double
    | Rectangulo Ponto Ponto
    | Triangulo Ponto Ponto Ponto
    deriving (Show, Eq)


triangula :: Poligonal -> [Figura]
triangula [] = error "Can't make triangles without points!"
triangula [x,y,z] = [Triangulo x y z]
triangula (x:y:z:xs) = (Triangulo x y z):triangula (x:z:xs)

--d
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
    

getArea :: Poligonal -> Double
getArea [] = 0
getArea p = getTriArea triangles
    where
        triangles = triangula p
        getTriArea [] = 0
        getTriArea (t:ts) = area t + getTriArea ts

--e
-- I don't get this one
mover :: Poligonal -> Ponto -> Poligonal
mover [x,y] p = [p,y]
mover (x:xs) p = p : mover xs p

--f
zoom :: Double -> Poligonal -> Poligonal
zoom = undefined
-- ...

--3
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String
    deriving Show

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
acrescemail :: Nome -> String -> Agenda -> Agenda
acrescemail n e [] = [(n, [Email e])]
acrescemail n e ((x,y):xs)
    | x == n = (x,(Email e):y) : xs
    | otherwise = (x,y):acrescemail n e xs

--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,y):xs)
    | n == x = Just (getEmails y)
    | otherwise = verEmails n xs
    where
        getEmails [] = []
        getEmails ((Email s):cs) = s : getEmails cs
        getEmails (_:cs) = getEmails cs

--c 
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Email x):xs) = consTelefs xs
consTelefs ((Casa x):xs) = x:consTelefs xs
consTelefs ((Tlm x):xs) = x:consTelefs xs
consTelefs ((Trab x):xs) = x:consTelefs xs

--d
casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing
casa n ((x,y):xs)
    | n == x = getCasa y
    | otherwise = casa n xs
    where
        getCasa [] = Nothing
        getCasa ((Casa x):xs) = Just x
        getCasa (_:xs) = getCasa xs

--4
type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String -- It's already defined above

data Data = D Dia Mes Ano
    deriving (Show,Eq)

type TabDN = [(Nome, Data)]

t1 :: TabDN
t1 = [("Mateus", D 12 10 2001), ("Mateus2", D 12 10 1990)]

--a
procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((x,y):xs)
    | n == x = Just y
    | otherwise = procura n xs

--b
idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D x1 y1 z1) n ((x,y):xs)
    | n /= x = Nothing
    | otherwise = 
        let (D x2 y2 z2) = y
        in if (y1==y2 && x1>=x2)||(y1>y2) then Just (z2-z1) else Just (z2-z1+1)
-- we could use the procura funtion but the exercise said not to use Eq in the Data type

--c
anterior :: Data -> Data -> Bool
anterior (D x1 y1 z1) (D x2 y2 z2)
    | (x1>=x2 && y1==y2 && z1==z2) = False
    | (y1>=y2 && z1==z2) = False
    | (z1>z2) = False
    | otherwise = True

--d 
ordena :: TabDN -> TabDN
ordena [] = []
ordena [x] = [x]
ordena ((n1,x):xs)
    | (anterior x y) == True = (n1,x) : ordena ((n2,y):xs)
    | otherwise = (n2,y) : ordena ((n1,x):ys)
    where
        ((n2,y):ys) = ordena xs 

--e
-- weird error on the lambda funtion
porIdade :: Data -> TabDN -> [(Nome, Int)]
porIdade d1 t =
    let
        ordTab = ordena t
        ordTabIdade = map (\(n,d2) -> (n,idade d1 n t)) ordTab
        tabi = map (\(n, Just d) -> (n,d)) ordTabIdade
    in 
        tabi

--5
data Movimento = Credito Float | Debito Float
    deriving Show

data Data2 = D2 Int Int Int
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

e1 :: Extracto
e1 = Ext 200 [((D 10 1 2020),"ab",Debito 10 ),((D 11 1 2020),"ab",Credito 9 ),((D 12 1 2020),"abc",Credito 10),((D 13 2 2019),"qwe",Debito 12)] 

--a
vMov :: Movimento -> Float
vMov (Credito x) = x
vMov (Debito x) = x

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext n []) _ = []
extValor (Ext n ((d,s,m):xs)) f
    | vMov m > f = m : extValor (Ext n xs) f
    | otherwise = extValor (Ext n xs) f

--b
filtro :: Extracto -> [String] -> [(Data, Movimento)]
filtro (Ext n []) sl = []
filtro (Ext n ((d,s,m):xs)) sl
    | (s `elem` sl) == True = (d,m) : filtro (Ext n xs) sl 
    | otherwise = filtro (Ext n xs) sl

--c
sumDebit :: Extracto -> Float
sumDebit (Ext n []) = 0
sumDebit (Ext n ((d,s,(Debito x)):xs)) = x + sumDebit (Ext n xs)
sumDebit (Ext n ((d,s,(Credito x)):xs)) = 0 + sumDebit (Ext n xs)

sumCredit :: Extracto -> Float
sumCredit (Ext n []) = 0
sumCredit (Ext n ((d,s,(Debito x)):xs)) = 0 + sumCredit (Ext n xs)
sumCredit (Ext n ((d,s,(Credito x)):xs)) = x + sumCredit (Ext n xs)

creDeb :: Extracto -> (Float, Float)
creDeb (Ext n []) = (0,0)
creDeb e = (sumCredit e, sumDebit e)

--d
saldo :: Extracto -> Float
saldo (Ext e l) = saldofinal
    where 
        (credit, debit) = creDeb (Ext e l)
        saldofinal = credit + e - debit

