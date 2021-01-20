import System.Random
import Data.Char

--1

--randomIO :: Random a => IO
--randomRIO :: Random a => (a,a) -> IO

--a
bingo :: IO()
bingo = do 
    n1 <- acumularNums []
    print n1

acumularNums :: [Int] -> IO [Int]
acumularNums l 
    | length l == 90 = do return l
    | otherwise = do 
        v <- randomRIO (1,90)
        print v 
        getChar
        if v `elem` l then acumularNums l else acumularNums (v:l)

--b
mastermind :: IO ()
mastermind = do 
    (a,b,c,d) <- criarString
    guess (a,b,c,d)
    return ()

criarString :: IO (Int, Int, Int, Int)
criarString = do 
    a <- randomRIO (0,9)
    b <- randomRIO (0,9)
    c <- randomRIO (0,9)
    d <- randomRIO (0,9)
    print "String Aleatório Criado!"
    return (a,b,c,d)

checkIsDigit :: [Char] -> Bool
checkIsDigit [] = False
checkIsDigit (x:xs) = if (toLower x) `elem` ['a'..'z'] then checkIsDigit xs else True

getGuess :: IO (Int, Int, Int, Int)
getGuess = do
    print "Escreva 4 letras: "
    x <- getLine
    let (a:b:c:d:resto) = x
    if length x /= 4 || checkIsDigit x
        then getGuess
    else
        return (read [a], read [b], read [c], read [d])

guess :: (Int, Int, Int, Int) -> IO ()
guess (n1,n2,n3,n4) = do
    let nums = [n1,n2,n3,n4]
    (g1,g2,g3,g4) <- getGuess
    let numsC = 0 + (if n1 == g1 then 1 else 0) + (if n2 == g2 then 1 else 0) + (if n3 == g3 then 1 else 0) + (if n4 == g4 then 1 else 0)
    let numsS = 0 + (if n1 /= g1 && g1 `elem` nums then 1 else 0) + (if n2 /= g2 && g2 `elem` nums then 1 else 0) + (if n3 /= g3 && g3 `elem` nums then 1 else 0) + (if n4 /= g4 && g4 `elem` nums then 1 else 0)
    if numsC /= 4 then print $ "Certos: " ++ show (numsC) ++ "\nCertos no sitio mal: " ++ show (numsS) else print "Ganhaste!"
    if numsC /= 4 then guess (n1, n2, n3, n4) else return ()

--2
data Aposta = Ap [Int] (Int, Int)

a1 :: Aposta
a1 = (Ap [1,2,3,4,5] (1,2))

a2 :: Aposta
a2 = (Ap [1,2,3,6,7] (1,1))

--a
checkN :: [Int] -> Bool
checkN [] = True
checkN (n:xs) = if n >= 1 && n <= 50 then checkN xs else False

checkS :: Int -> Bool
checkS n = if n >= 1 && n <= 9 then True else False

valida :: Aposta -> Bool
valida (Ap ns (y,z))
    | length ns == 5 && checkN ns && checkS y && checkS z = True
    | otherwise = False

--b
seeNs :: [Int] -> [Int] -> Int
seeNs [] _ = 0
seeNs _ [] = 0
seeNs (x:xs) l = if x `elem` l then 1 + seeNs xs l else seeNs xs l

seeEs :: (Int, Int) -> (Int, Int) -> Int
seeEs (x1,x2) l = 0 + (if x1 `isIn` l then 1 else 0) + (if x2 `isIn` l then 1 else 0)

isIn :: Int -> (Int, Int) -> Bool
isIn n (x,y) = if n == x || n == y then True else False

comuns :: Aposta -> Aposta -> (Int, Int)
comuns (Ap n1 e1) (Ap n2 e2) = (seeNs n1 n2, seeEs e1 e2)

--c
instance Eq Aposta where
    a1 == a2 = if comuns a1 a2 == (5,2) then True else False 

premio :: Aposta -> Aposta -> Maybe Int
premio ap ch = case comuns ap ch of (5,n) -> Just (3 - n)
                                    (4,n) -> Just (6 - n)
                                    (3,n) -> Just (10 - n - (if n == 2 then 1 else 0))
                                    (2,2) -> Just 8
                                    (1,2) -> Just 11
                                    (2,n) -> Just (13 - n)
                                    otherwise -> Nothing

--d
lerNums :: [Char] -> IO [Char]
lerNums nums = do
    if (length nums) == 7 
        then return nums 
    else do
        x <- getLine
        lerNums (nums++x)

-- leAposta :: IO Aposta
-- leAposta = do
--     (a,b,c,d,e,f,g) <- lerNums []
--     let nums = [a,b,c,d,e]
--     let stars = [f,g]
--     print $ "Numeros: " ++ show (nums) ++ " Estrelas: " ++ show (stars)

-- leAposta = do
--     print "Introduza os numeros (separados por um espaco):"
--     nums <- getLine
--     print "Introduza as estrelas (separadas por um espaco):"
--     stars <- getLine
--     let bet = (Ap (map (read) (unspace nums)) (let (a:b:r) = (unspace stars) in (read a, read b)))
--     if valida bet then return bet else do print "Aposta invalida, tente novamente!"; leAposta
