import Data.List

--1
data Frac = F Integer Integer

instance Eq Frac where
    (F a b) == (F c d) = a * d == b * c

instance Ord Frac where
    (F a b) <= (F c d) = a * d <= b * c
    --(F a b) >= (F c d) = a * d >= b * c

instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

instance Num Frac where
    --class (Eq a, Show a) => Num a where
    --(+), (*), (-) :: a -> a -> a
    --negate, abs, signum :: a -> a
    --fromInteger :: Integer -> a

    (F a b) + (F c d)
        | b == d = normaliza $ F (a + c) b
        | otherwise = normaliza $ F (a * d + b * c) (b * d)
    (F a b) - (F c d) = (F a b) + negate (F c d)
    (F a b) * (F c d) = (F (a*c) (b*d))
    negate (F a b) = (F (-a) b)
    abs (F a b) = (F (abs a) (abs b))
    signum (F a b)
        | a == 0 = 0
        | a * b > 0 = 1
        | otherwise = (-1)
    fromInteger x = (F x 1)

--mdc x y == mdc (x+y) y == mdc x (y+x)
mdc :: Integer -> Integer -> Integer
mdc x y
    | x == y = x
    | x < y = mdc x (y-x)
    | otherwise = mdc y x

--a
normaliza :: Frac -> Frac
normaliza (F x y)
    | mxy == 1 = (F x y)
    | otherwise = normaliza (F (x `div` mxy) (y `div` mxy))
    where
        mxy = mdc (abs x) (abs y)

--3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)] 

-- ?????? idk how to do this one without some really weird functions,
-- which I don't think I'm supoposed to use
instance Show Extracto where
    show (Ext f l) = 
        "Saldo anterior: " ++ show f ++
        "\n----------------------------------------------" ++
        "\n      Data     Descricao    Credito     Debito" -- ++
        --"\n " ++ map (show) l

e1 :: Extracto
e1 = Ext 200 [((D 10 1 2020),"ab",Debito 10 ),((D 11 1 2020),"ab",Credito 9 ),((D 12 1 2020),"abc",Credito 10),((D 13 2 2019),"qwe",Debito 12)] 

instance Ord Data where
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | ano1 > ano2 || (ano1 == ano2 && mes1 > mes2) || (ano1 == ano2 && mes1 == mes2 && dia1 > dia2) = GT 
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
        | otherwise = LT

instance Show Data where
    show (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a
    --show (D dia mes ano) = concat $ intersperse "/" $ map (show) [dia,mes,ano]

-- na Data.List há o sortBy
ordena :: Extracto -> Extracto
ordena (Ext f l) = (Ext f (sortBy (\(a,_,_) (b,_,_) -> compare a b) l))


