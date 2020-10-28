module Generator where

import System.Random

type Labirinto = [Corredor]
type Corredor = [Peca]
data Peca = Comida TamanhoComida | Parede | Chao
data TamanhoComida = Grande | Pequena

instance Show Peca where
    show (Comida Grande) = "o"
    show (Comida Pequena) = "."
    show (Parede) = "#"
    show (Chao) = " "

sampleMaze :: Labirinto
sampleMaze = [
                [Parede, Parede, Parede, Parede, Parede, Parede, Parede, Parede],
                [Chao, Comida Pequena, Comida Pequena, Comida Grande, Comida Pequena, Comida Grande, Comida Pequena, Chao],
                [Parede, Parede, Parede, Parede, Parede, Parede, Parede, Parede]
            ]


-- | Given a seed returns a list of n integer randomly generated
--
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,9) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Given a seed returns an integer randomly generated
--
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- Converssta list into a list of list of size n
--
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | Converts an integer number into a Peca
-- 3 <=> Comida Grande
-- 0 <= n < 7 <=> Comida Pequena
-- 7 < n <= 9 <=> Parede
--
convertePeca :: Int -> Peca
convertePeca n | n == 3 = Comida Grande
               | n >= 0 && n < 7 = Comida Pequena
               | n >= 7 && n <= 9 = Parede         


-- | Converts a Corredor to a string
--
printCorridor :: Corredor -> String
printCorridor [] = "\n"
printCorridor (x:xs) = show x ++ printCorridor xs -- show returns a string so we can't prepend it ????


-- | Converts a Labirinto to a string
--
printMaze :: Labirinto -> String
printMaze [] = []
printMaze (x:xs) = printCorridor x ++ printMaze xs


-- | Converts a list of integers into a Corredor
--
converteCorredor :: [Int] -> Corredor --Corredor = [Pecas]
converteCorredor [] = []
converteCorredor (h : t) = (convertePeca h) : (converteCorredor t)


-- | Converts a list of lists of integers into a Labirinto
--
converteLabirinto :: [[Int]] -> Labirinto
converteLabirinto [] = []
converteLabirinto (x : xs) = converteCorredor x : converteLabirinto xs


geraLabirinto :: Int -> Int -> Int -> IO ()
geraLabirinto x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in putStrLn $ printMaze $ converteLabirinto $ subLista x random_nrs

