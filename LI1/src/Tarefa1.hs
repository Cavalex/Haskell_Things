{- |

= Introdução

A tarefa 1 foi o nosso primeiro contacto com o trabalho e com todos os novos tipos de dados. Foi, por isso bastante desafiante entrar dentro
desta tarefa, e conseguir adaptarmo-nos ao ponto de fazer o nosso melhor. Nesta tarefa teríamos que começar por fazer uma labirinto 
que cumprisse todas as diretrizes impostas. Não sendo isso tão linear como pensamos. 

= Objetivos

O principal objetivo seria fazer um labirinto que cumprisse todas as normas para ser um labirinto válido, assim teria de ter uma casa de fantasmas,
comidas aleatórias, um túnel no meio do labirinto e todos as outras peças que estão no jogo original. 
Sendo que este foi o nosso principal foco dividimos por partes toda a composição do labirinto para o podermos juntar no fim. Assim, fomos fazendo as paredes,
o túnel, e deixando a casa por último que foi o mais desafiante para nós. Juntamos tudo no final e obtivemos um labirinto válido

= Discussão e conclusão 

Ãpesar das primeiras dificuldades de ser tudo novo para nós, adaptamo-nos bem a todos os tipos e consegui-mos o resultado que pretendiamos. 

-}

module Tarefa1 where

import Types

import System.Random


-- * 'Maze's de teste 

m1 :: Maze
m1 = (generateMaze 30 20 24)

m2 :: Maze
m2 = (generateMaze 31 21 24)

-- * Desenvolvimento da Tarefa 1

-- | Exemplo de um 'Maze' 

exampleMaze = [
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
    [Empty, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Empty],
    [Empty, Food Little, Food Little, Food Little, Food Little, Wall, Food Little, Empty],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
    ]

-- | Cria uma lista aleatória de inteiros, em que primeiro criamos um gerador aleatório, depois pegando nos __n__ primeiros elementos de uma serie infinita de números entre o __0__ e o __99__. 

generateRandoms :: Int   -- ^ Número de 'Int's aleatórios a criar
                -> Int   -- ^ Seed para a criação da lista de 'Int's aleatórios
                -> [Int] -- ^ Lista de 'Int's aleatórios
generateRandoms n seed = let gen = mkStdGen seed 
                        in take n $ randomRs (0,99) gen 

-- | Escreve num certo __índice__ de um certo 'Maze' a 'Piece' que atribuimos e retorna-nos um 'Maze' com essa mesma 'Piece' já colocada no respetivo __índice__.

writePieceOnIndices :: Coords -- ^ Apesar de serem 'Coords' são índices no 'Maze'
                    -> Piece  -- ^ A 'Piece' a substituir 
                    -> Maze   -- ^ O 'Maze' antigo
                    -> Maze   -- ^ O novo 'Maze'
writePieceOnIndices (n1, n2) p [] = []
writePieceOnIndices (n1, n2) p (xw:xws)
    | n2 <= 0 = (redrawLine n1 p xw) : xws
    | otherwise = xw : (writePieceOnIndices (n1, (n2-1)) p xws)
    where 
        redrawLine 0 p (xll:xlls) = p : xlls
        redrawLine xl p (xll:xlls) = xll : (redrawLine (xl-1) p (xlls))

-- | Numas certas 'Coords' do nosso 'Maze' escrevemos uma 'Piece' em específico, ou seja indicamos umas certas 'Coords', uma certa 'Piece' e um qualquer 'Maze', a função irá reescrever o 'Maze' colocando essa 'Piece' nessas 'Coords'.

-- | A diferença entre este e o anterior reside no facto de o anterior se referir a índices e este se referir a 'Coords'. 

writePieceOn :: Coords -- ^ São dadas umas 'Coords' 
             -> Piece  -- ^ A 'Piece' a substituir
             -> Maze   -- ^ O 'Maze' antigo
             -> Maze   -- ^ O novo 'Maze'
writePieceOn (n1, n2) p m = writePieceOnIndices ((n1-1), (n2-1)) p m

-- | O objetivo é reescrevermos um __"x"__ números de 'Piece's de uma certa linha, assim atribuimos umas 'Coords' e a partir dessas 'Coords' até ao fim das __"x"__ 'Piece's dessa linha ela vai reescreve-las.

writeLine :: Coords -- ^ Recebe umas 'Coords' 
          -> Int    -- ^ Um 'Int' que será a quantidade de vezes que repetirá a ação 
          -> Piece  -- ^ Uma 'Piece' que será com ela que reescreverá
          -> Maze   -- ^ O 'Maze' antigo
          -> Maze   -- ^ O novo 'Maze' 
writeLine (xl1, yl) xl2 p m
    | xl1 == xl2 = m
    | otherwise =  writePieceOn (xl1, yl) p (writeLine ((xl1+1),yl) xl2 p m)

-- | Usa a 'writeLine' para desenhar um quadrado de uma certa 'Piece' numas certas 'Coords'.

writeSquare :: Coords -- ^ As 'Coords' serão sempre o ínicio do nosso retângulo, estando sempre à esquerda das próximas 'Coords'
            -> Coords -- ^ Já estas 'Coords' serão o fim do nosso retângulo 
            -> Piece  -- ^ A 'Piece' que vamos aplicar a todo o nosso retângulo 
            -> Maze   -- ^ O 'Maze' antigo
            -> Maze   -- ^ O 'Maze' novo
writeSquare (xl1, yl1) (xl2, yl2) p m
    | yl1 == yl2 = m
    | otherwise = writeLine (xl1, yl1) xl2 p (writeSquare (xl1, (yl1+1)) (xl2, yl2) p m)

-- | Cria as __paredes laterais__ que delimitam o 'Maze' aleatório, sendo que a 'Piece' __Wall__ corresponde ao Valor entre __70__ a __99__ colocamos o __99__ no ínicio e no final da lista. 

genRandomsForCorridor :: Int   -- ^ Número de 'Int's aleatórios a criar
                      -> Int   -- ^ Seed para a criação da lista de 'Int's aleatórios
                      -> [Int] -- ^ Lista de 'Int's aleatórios
genRandomsForCorridor n seed = 99 : (generateRandoms n seed) ++ [99]

-- | Serve para criar os __túneis__ dentro do nosso 'Maze', acontece o mesmo que no anterior só que com o valor __100__.

genRandomsForTunnel :: Int   -- ^ Número de 'Int's aleatórios a criar
                    -> Int   -- ^ Seed para a criação da lista de 'Int's aleatórios
                    -> [Int] -- ^ Lista de 'Int's aleatórios
genRandomsForTunnel n seed = 100 : (generateRandoms n seed) ++ [100]

-- | Esta função serve como um ponto de partida para a gerar as 'Piece's no 'Maze', mas, nesta ainda são representadas por __números__, sendo numa fase posterior passadas a 'Piece's. 

-- |  Dividimos o argumento para o interpretador saber onde pôr o __túnel__. 

genNums :: Int        -- ^ Argumento que atribui a altura ao 'Maze'
        -> Int        -- ^ Argumento que atribui o comprimento ao 'Maze'
        -> (Int, Int) -- ^ São as linhas onde vamos colocar __túnel__ 
        -> Int        -- ^ Seed para a criação 
        -> [[Int]]    -- ^ Produto final, uma lista de listas de 'Int's aleatórios de acordo com os argumentos anteriores
genNums x1 0 split seed = []
genNums x1 y1 (c1, c2) seed
    | c1 == y1 = genTunnelLine
    | c2 == y1 = genTunnelLine
    | otherwise = genNextLine
    where 
        genNextLine = (genRandomsForCorridor (x1-2) seed):(genNums x1 (y1-1) (c1,c2) (seed+1))
        genTunnelLine = (genRandomsForTunnel (x1-2) seed):(genNums x1 (y1-1) (c1,c2) (seed+1))

-- |  Aqui acontece a transição de um __número__ para uma 'Piece'.

convertPiece :: Int   -- ^ Recebe um qualquer número inteiro
             -> Piece -- ^ Retorna a 'Piece' equivalente ao 'Int'
convertPiece n
    | n == 3 = Food Big
    | n >= 0 && n < 70 = Food Little
    | n >= 70 && n <= 99 = Wall
    | n == 100 = Empty 

-- | Usa a função 'convertPiece' para converter um 'Corridor' numa lista de 'Pieces'

convertCorridor :: [Int]    -- ^ Recebe uma lista de 'Int's
                -> Corridor -- ^ Para cada 'Int' da lista retorna a 'Piece' equivalente
convertCorridor [] = []
convertCorridor (h:t) = (convertPiece h):(convertCorridor t)

-- | Por fim esta aproveita-se da anterior 'convertCorridor' para trocar os números de todo o 'Maze' para 'Piece's.

convertMaze :: [[Int]] -- ^ Recebe uma lista de lista de 'Int's
            -> Maze    -- ^ Retorna um 'Maze' já com todos os 'Corridor's passados a 'Piece's
convertMaze [] = []
convertMaze (c:cs) = (convertCorridor c):(convertMaze cs)


-- | Para gerar a casa dos fantasmas tudo foi feito em vários passos, desde criar o espaço vazio em volta da casa, criar as paredes da casa, criar o espaço vazio dentro da casa e por fim as portas.

-- | * __genEmptySpace__ - Em primeiro lugar atua esta função que gera um espaço vazio no meio do 'Maze'. Resumidamente, esta encontra o meio do 'Maze' e "desenha" um retângulo de 'Piece's __Empty__, 10x5 se o comprimento do 'Maze' for par ou 11x5 se for ímpar.

-- | * __genHouseWalls__ - Em segundo lugar criam-se as paredes da casa, ao fazer isto acabamos por criar um retângulo de 'Piece's __Wall__ que varia de tamanho conforme o 'Maze' seja par ou ímpar, ficando com 8 ou 9 de comprimento, sendo a altura sempre 3.

-- | * __genLivingRoom__ - Após termos este retângulo de 'Piece's __Wall__ voltamos a desenhar um espaço de 'Piece's __Empty__, mas agora dentro do quadrado que foi criado anteriormente.

-- | * __genDoors__ - Procura o meio da parede superior retirando-lhe 2 ou 3 espaços, conforme a casa seja par ou ímpar respetivamente.

genHouse :: Maze -> Maze
genHouse m = genDoors $ genLivingRoom $ genHouseWalls $ genEmptySpace m
    where
        genDoors m 
            | lx `mod` 2 /= 0 = writeLine (xe1,fstY) (xe1+3)  Empty m
            | otherwise = writeLine (xe2,fstY) (xe2+2) Empty m
            where
                (xh:xhs) = m
                lx = length xh
                ly = length m
                xe1 = ((lx-3) `div` 2) + 1
                xe2 = ((lx-2) `div` 2) + 1
                fstY = 
                    if ly `mod` 2 == 0
                        then (ly-4) `div` 2 + 1
                        else (ly-3) `div` 2 + 1

        genLivingRoom m -- lol
            | lx `mod` 2 /= 0 = writeLine (xe1, fstY) (xe1+7)  Empty m
            | otherwise = writeLine (xe2, fstY) (xe2+6) Empty m
            where
                (xh:xhs) = m
                lx = length xh
                ly = length m
                xe1 = ((lx-7) `div` 2) + 1
                xe2 = ((lx-6) `div` 2) + 1
                fstY = 
                    if ly `mod` 2 == 0
                        then (ly-2) `div` 2 + 1
                        else (ly-1) `div` 2 + 1

        genHouseWalls m
            | lx `mod` 2 /= 0 = writeSquare (xe1, fstY) ((xe1+9), (fstY+3)) Wall m
            | otherwise = writeSquare (xe2, fstY) ((xe2+8), (fstY+3)) Wall m
            where
                (xh:xhs) = m
                lx = length xh
                ly = length m
                xe1 = ((lx-9) `div` 2) + 1
                xe2 = ((lx-8) `div` 2) + 1
                fstY = 
                    if ly `mod` 2 == 0
                        then (ly-4) `div` 2 + 1
                        else (ly-3) `div` 2 + 1
        
        genEmptySpace m
            | lx `mod` 2 /= 0 = writeSquare (xe1, fstY) ((xe1+11), (fstY+5)) Empty m
            | otherwise = writeSquare (xe2, fstY)  ((xe2+10), (fstY+5)) Empty m
            where
                (xh:xhs) = m
                lx = length xh
                ly = length m
                xe1 = ((lx-11) `div` 2) + 1 
                xe2 = ((lx-10) `div` 2) + 1
                fstY = 
                    if ly `mod` 2 == 0 
                        then (ly-6) `div` 2 + 1
                        else (ly-5) `div` 2 + 1


-- |  Vai criar o nosso 'Maze', dado um __comprimento__, __altura__ e uma __seed__ (que vai servir para o nosso 'Maze' ser aleatório).

-- |  É também nesta função que são adicionadas as paredes superiores e inferiores.

generateMaze :: Int   -- ^ Comprimento do 'Maze'
             -> Int   -- ^ Altura do 'Maze'
             -> Int   -- ^ Seed para o gerador de números aleatórios
             -> Maze  -- ^ 'Maze' criado de acordo com os parâmetros passados acima 
generateMaze x y seed 
    | y `mod` 2 == 0 = genHouse $ convertMaze (wallRow:(genNums x (y-2) (y `div` 2, (y `div` 2)-1) seed) ++ [wallRow])
    | otherwise = genHouse $ convertMaze (wallRow:(genNums x (y-2) (((y+1) `div` 2)-1, -1) seed) ++ [wallRow])
    where
        wallRow = (take x (repeat 99))


-- | Substitui o 'Corridor' por uma 'String' 

corridorToString :: Corridor  -- ^ É dado um 'Corridor' qualquer
                 -> String    -- ^ É retribuída uma 'String' correspondente àquele 'Corridor'
corridorToString [] = []
corridorToString (x:xs) = show x ++ corridorToString xs

-- | Esta usa o anterior e converte todo o 'Maze' numa 'String'

mazeToString :: Maze   -- ^ É dado um 'Maze' qualquer
             -> String -- ^ É retribuída uma 'String' correspondente àquele 'Maze'
mazeToString [] = []
mazeToString (x:xs) = corridorToString x ++ "\n" ++ mazeToString xs

