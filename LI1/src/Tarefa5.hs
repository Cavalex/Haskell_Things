{- |

= Introdução 

Foi nesta tarefa que tivemos a primeora experiência com inteligência artificial dentro do trabalho. Sendo por isso uma das tarefas maios complicadas 
do trabalho. Apesar de ser díficil é sempre desafiante fazer algo deste género. Após umas largas horas de testes e de escrita de código fomos aprendendo
como é que poderíamos fazer acontecer.


= Objetivos 

O grande objetivo da tarefa passava por fazer com que os fantasmas decidissem a sua próxima jogada tendo em conta todas as variáveis que os rodeiam. 
Assim sendo, os nossos fantasmas poderiam adotar dois tipos de comportamentos, uma de fugir e outra de andar atrás do pacman, dependendo do modo em que 
se encontrasse. Bastava então perceber como é que colocariamos isso no código. Decidimos então fazer dois tipos de algoritmos, um deles composto por uma 
RTree, assim ele poderia tomar a sua decisão conforme a sua distância ao pacman e o seu modo. E muito resumidamente foi assim que conseguimos programar os 
nossos fantasmas para fazer o que lhes competia.


= Discussão e conclusão

Sendo que esta, já como acima referido, foi uma das tarefas mais dificeis de concluir fomos tendo alguns erros, maas conseguimos ultrapassa-los, fazendo com
que o fantasma agisse como pretendido

-}


module Tarefa5 where

import Types
import System.Random
import Data.List
import Tarefa2

jogador3 :: Player
jogador3 = Pacman (PacState (1,(1,3),2,U,10,1) 0 Open Normal)

enemy3 :: Player
enemy3 = Ghost (GhoState (3,(3,3),2,U,0,1) Alive)

testMaze :: Maze
testMaze = 
    [[Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Wall],
    [Wall,Food Little,Wall,Wall,Wall,Food Little,Wall],
    [Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall]]

testState :: State
testState = createInitialState [jogador3] [enemy3] testMaze

-- * Funções principais

data RTree a = T a [RTree a] deriving Show

--path :: RTree (Coords, Orientation)
--path = T ((-1, -1), R) []

-- profundidade de procura nas RTrees
-- funciona com 12, mas o ideal é colocar 10
profundidade :: Int -- ^ Uma constante
profundidade = 9

-- | Preenche as folhas da 'RTree'
fillLeaf :: Maze                        -- ^ O 'Maze'  
         -> RTree (Coords, Orientation) -- ^ A 'RTree' inicial 
         -> RTree (Coords, Orientation) -- ^ A 'RTree' com as folhas preenchidas
fillLeaf maze (T (c,o) []) = T (c,o) (getNeighboursTree maze c)

-- | ...
fillPath :: Coords                      -- ^ 'Coords' alvo
         -> Int                         -- ^ A profundidade
         ->  Maze                       -- ^ O 'Maze'
         -> [Coords]                    -- ^ A lista de todas as coordenadas dos "pais" 
         -> RTree (Coords, Orientation) -- ^ A 'RTree' inicial
         -> RTree (Coords, Orientation) -- ^ A 'RTree' final
fillPath ca pf maze parents (T (c,o) trees) = if pf <= 0 || areCoordsEqual c ca || c `elem` parents then (T (c,o) trees) else (T (c,o) (map (fillPath ca (pf-1) maze (c:parents)) (map (fillLeaf maze) trees)))

-- | Compara duas 'Coords' e vê se elas são iguais 
areCoordsEqual :: Coords -- ^ Umas quaisquer 'Coords'
               -> Coords -- ^ Outras 'Coords' quaisqueres 
               -> Bool   -- ^ O 'Bool' correspondente 
areCoordsEqual (x,y) (w,z) = x == w && y == z

-- | Escolhe o próximo movimento do Bot
getNextMove :: Int         -- ^ O que irá decidir o comportamento do bot
            -> Maze        -- ^ O 'Maze' 
            -> Orientation -- ^ A 'Orientation'
            -> [Coords]    -- ^ 'Coords' da casa dos fantasmas
            -> Coords      -- ^ 'Coords' iniciais
            -> Coords      -- ^ 'Coords' alvo
            -> Orientation -- ^ A 'Orientation' final
getNextMove chaseScatter maze o houseCoords ci ca = if ci == ca || ls == 2 then changeMethod else d    
    where
        (h1:h2:(x,y):hs) = houseCoords
        ca2 = if ci `elem` houseCoords then (x-2, y) else ca
        paths = pathsToNode (ca2,R) $ fillPath ca2 profundidade maze [] (fillLeaf maze (T (ci, o) []))
        smallestPath = if length paths == 0 then [(ci, Null), (ci, Null)] else getSmallestPath paths $ head paths
        ls = length smallestPath
        changeMethod = evaluateMoves chaseScatter maze ci (getNeighbours maze houseCoords ci) (-1,ca)
        ((a,b):(c,d):xs) = smallestPath

-- | Escolhe o menor array dentro de um conjunto de arrays
getSmallestPath :: [[a]] -- ^ O conjunto de arrays
                -> [a] -- ^ Um acumulador
                -> [a] -- ^ O array mais pequeno
getSmallestPath [] ac = ac
getSmallestPath (x:xs) ac = if length x < length ac then getSmallestPath xs x else getSmallestPath xs ac

-- | Recolhe a informação sobre o primeiro tuplo da 'RTree'
getHeadInfo :: RTree (Coords, Orientation) -- ^ Uma 'RTree'  
            -> (Coords, Orientation)       -- ^ As 'Coords' e 'Orientation' do primeiro elemento
getHeadInfo (T (c,o) trees) = (c,o)

-- | Descobre a distância entre duas 'Coords' devolvendo a distância e a 'RTree' dada
getDistanceTo :: Coords                      -- ^ Umas 'Coords'
              -> RTree (Coords, Orientation) -- ^ Uma 'RTree' 
              -> (Double, RTree (Coords, Orientation)) -- ^ Um tuplo da distância e da 'RTree'
getDistanceTo c1 (T (c2,o) trees) = (distance c1 c2, (T (c2,o) trees))

-- | Retorna o caminho para um determinado node numa RTree
pathsToNode :: (Coords,Orientation)  -- Um tuplo de 'Coords' e 'Orientation'
            -> RTree (Coords, Orientation) -- ^ Uma 'RTree'
            -> [[(Coords, Orientation)]] -- ^ A lista de 'Coords' e 'Orientation'
pathsToNode ca (T y ns) = [[ca] | (fst ca) == (fst y)] ++ map (y:) (pathsToNode ca =<< ns)

-- | Descobre as 'Coords' de todos os 'Player's
getAllPlayersCoords :: State    -- ^ Um 'State'
                    -> [Coords] -- ^ A lista das 'Coords' de todos os 'Player's
getAllPlayersCoords (State maze playerList level) = aux playerList
    where
        aux [] = []
        aux ((Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)):xs) = (xc,yc):aux xs
        aux (_:xs) = aux xs

-- | Calcula a distância entre duas 'Coords'
distance :: Coords -- ^ As 'Coords' de um 'Ghost'
         -> Coords -- ^ As 'Coords' de um 'Player'
         -> Double -- ^ A distância entre eles
distance (xa1,ya1) (xa2,ya2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
    where
        x1 = fromIntegral xa1
        y1 = fromIntegral ya1
        x2 = fromIntegral xa2
        y2 = fromIntegral ya2

-- | Consegue a árvore dos vizinhos das 'Coords' anteriores
getNeighboursTree :: Maze    -- ^ Um 'Maze'
                  -> Coords  -- ^ Umas 'Coords'
                  -> [RTree (Coords, Orientation)] -- ^ Uma 'RTree'  dos vizinhos das 'Coords' anteriores
getNeighboursTree maze (x,y) = aux maze [
        T (updatePos maze (x,y) [D], D) [],
        T (updatePos maze (x,y) [U], U) [],
        T (updatePos maze (x,y) [L], L) [],
        T (updatePos maze (x,y) [R], R) []
        ]
    where
        aux _ [] = []
        aux maze ((T (c,o) []):xs) = if (checkPiece c maze) == Wall then aux maze xs else (T (c,o) []):aux maze xs

-- | Descobre todos os vizinhos 
getNeighbours :: Maze  -- ^ Um 'Maze'
              -> [Coords] -- ^ Uma lista de 'Coords'
              -> Coords  -- ^ Umas 'Coords' 
              -> [(Coords, Orientation)] -- ^ Um tuplo com 'Coords' e 'Orientation'
getNeighbours maze houseCoords (x,y) = aux maze [
    (updatePos maze (x,y) [D], D),
    (updatePos maze (x,y) [U], U),
    (updatePos maze (x,y) [L], L),
    (updatePos maze (x,y) [R], R)
    ]
    where
        aux _ [] = []
        aux maze (((a,b),o):xs) = if checkPiece (a,b) maze == Wall then aux maze xs else ((a,b),o):(aux maze xs)
        --aux maze (((a,b),o):xs) = if checkPiece (a,b) maze == Wall || (a,b) `elem` houseCoords then aux maze xs else ((a,b),o):(aux maze xs)

-- | Determina a altura da 'RTree'
alturaRTree :: RTree (Coords, Orientation) -- ^ Uma 'RTree'
            -> Int -- ^ A sua altura
alturaRTree (T a []) = 1
alturaRTree (T a l) = 1 + maximum (map alturaRTree l)

evaluateMoves :: Int -> Maze -> Coords -> [(Coords, Orientation)] -> (Int, Coords) -> Orientation
evaluateMoves chaseScatter maze coGhost (x:xs) (idP, coPlayer) = getOrientationFromTuple chaseScatter $ map (\(c,o) -> (distance c coPlayer, o)) (x:xs)

-- teste: getNextPlay playerList 0 3 maze L (3,3) (getNeighbours maze (getHouseCoords maze) (3,2)) (getClosest playerList (3,2) (-1) (1000,1000)) (getHouseCoords maze)
-- | Gera a próxima jogada de um 'Player'
getNextPlay :: [Player]  -- ^ Uma lista de 'Player's
            -> Int -- ^ O que irá decidir o comportamento do bot
            -> Int -- ^ O Id do 'Ghost'
            -> Maze -- ^ Um 'Maze'
            -> Orientation -- ^ A 'Orientation'
            -> Coords -- ^ As 'Coords' do 'Ghost'
            -> [(Coords, Orientation)] -- ^ Uma lista de tuplos de 'Coords' e 'Orientation'
            -> (Int, Coords) -- ^ O Id e 'Coords' do 'Player 
            -> [Coords] -- As 'Coords' da casa dos fantasmas 
            -> Play -- ^ O 'Play'
getNextPlay playerList chaseScatter ghostID maze o coGhost (x:xs) (idP,coPlayer) houseCoordinates
    | nextMoveChase == Null || (chaseScatter == 1) = Move ghostID nextMoveScatter
    | otherwise = Move ghostID nextMoveChase
    where
        (h1:h2:(hx,hy):hs) = houseCoordinates
        coAlvo = if coGhost `elem` houseCoordinates then (hx-2,hy) else coPlayer
        nextMoveChase = getNextMove chaseScatter maze o houseCoordinates coGhost coAlvo
        nextMoveScatter = evaluateMoves chaseScatter maze coGhost (x:xs) (idP,coPlayer)

-- | Tira a 'Orientation' de um tuplo 
getOrientationFromTuple :: Int -- ^ O mesmo int que determina a procura no tuple
                        -> [(Double,Orientation)] -- ^ Uma lista de tuplos 
                        -> Orientation -- ^ A 'Orientation' obtida do tuplo
getOrientationFromTuple sb list = aux smallestBiggest list
    where
        distances = map (\(x,y) -> x) list
        -- se for 0 escolhemos a distancia mais próxima do jogador, senão escolhemos a que está mais longe para fugir
        smallestBiggest = if sb == 0 then minimum distances else maximum distances
        aux _ [] = error "nao encontrou distancia na função getOrientationFromTuple"
        aux n ((d,o):xs) = if n == d then o else aux n xs

-- | Tira a 'Orientation' de um Id 
getOrientationByID :: [Player] -- ^ Uma lista de 'Player's 
                   -> Int  -- O Id do 'Player'
                   -> Orientation -- A 'Orientation' do 'Player'
getOrientationByID (x:xs) id = if getID x == id then getOrientation x else getOrientationByID xs id

-- | A partir do Id de um 'Player' conseguir as 'Coords' desse mesmo player
getCoordsByID :: [Player] -- ^ A lista de 'PLayer's
              -> Int      -- ^ O ID
              -> Coords   -- ^ As 'Coords' correspondentes ao Id
getCoordsByID (x:xs) id = if getID x == id then getCoords x else getCoordsByID xs id

-- | A partir do ID de um 'Player' conseguir o seu 'PacMode'
getPacModeByID :: [Player] -- ^ A lista de 'PLayer's
               -> Int      -- ^ O ID
               -> PacMode  -- ^ As 'Coords' correspondentes ao Id
getPacModeByID ((Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc pacmode):xs)) id = if ip == id then pacmode else getPacModeByID xs id
getPacModeByID (_:xs) id = getPacModeByID xs id
getPacModeByID [] _ = error "Player not found on getPacModeByID defined on tarefa5"

-- | Encontra o 'Ghost' mais próximo das 'Coords' dadas
getClosestGhost :: [Player] -- ^ A lista de 'Player's
           -> Coords   -- ^ As 'Coords'
           -> Int      -- ^ O acumulador do ID
           -> Coords   -- ^ O acumulador das 'Coords'
           -> (Int, Coords) -- ^ Um tuplo com o ID e as 'Coords' do 'Ghost'
getClosestGhost [] _ acID acD = (acID, acD)
getClosestGhost ((Ghost (GhoState (ip,(xc,yc),vel,op,score,lives) gMode)):xs) co acID acD
    | d < dAc = getClosestGhost xs co ip (xc,yc)
    | otherwise = getClosestGhost xs co acID acD
    where
        d = distance (xc,yc) co
        dAc = distance acD co 
getClosestGhost (_:xs) co acID acD = getClosestGhost xs co acID acD

-- | Encontra o 'Pacman' mais próximo das 'Coords' dadas
getClosestPlayer :: [Player] -- ^ A lista de 'Player's
           -> Coords -- ^ As 'Coords'
           -> Int -- ^ O acumulador do ID
           -> Coords -- ^ O acumulador das 'Coords'
           -> (Int, Coords) -- ^ Um tuplo com o ID e as 'Coords' do 'Pacman'
getClosestPlayer [] _ acID acD = (acID, acD)
getClosestPlayer ((Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)):xs) co acID acD
    | d < dAc = getClosestPlayer xs co ip (xc,yc)
    | otherwise = getClosestPlayer xs co acID acD
    where
        d = distance (xc,yc) co
        dAc = distance acD co
getClosestPlayer (_:xs) co acID acD = getClosestPlayer xs co acID acD

-- | Neste modo ele vai perseguir o Pacman
chaseMode :: State  -- ^ Um 'State'
          -> [Coords] -- ^ As 'Coords' da casa dos fantasmas
          ->  Int  -- ^ O Id
          -> Play -- ^ O 'Play'
chaseMode (State maze playerList level) houseCoords id = getNextPlay playerList 0 id maze ghostOrientation ghostCoords (getNeighbours maze houseCoords ghostCoords) (getClosestPlayer playerList ghostCoords (-1) (1000,1000)) houseCoords
    where
        ghostCoords = getCoordsByID playerList id
        ghostOrientation = getOrientationByID playerList id

-- | Neste modo ele vai fugir do Pacman
scatterMode :: State  -- ^ Um 'State'
            -> [Coords] -- ^ As 'Coords' da casa dos fantasmas
            -> Int  -- ^ O Id
            -> Play -- ^ O 'Play'
scatterMode (State maze playerList level) houseCoords id = getNextPlay playerList 1 id maze ghostOrientation ghostCoords (getNeighbours maze houseCoords ghostCoords) (getClosestPlayer playerList ghostCoords (-1) (1000,1000)) houseCoords
    where
        ghostCoords = getCoordsByID playerList id
        ghostOrientation = getOrientationByID playerList id

-- | Onde o ghost efetua as jogadas
ghostPlay :: State  -- ^ Um 'State'
          -> [Play] -- ^ A lista de 'Plays'
ghostPlay (State maze playerList level) = aux playerList
    where
        houseCoordinates = getHouseCoords maze
        s = (State maze playerList level)
        aux [] = []
        aux ((Ghost (GhoState (id,(xc,yc),vel,op,score,lives) gMode)):xs)
            | gMode == Dead = (scatterMode s houseCoordinates id):aux xs
            | otherwise = (chaseMode s houseCoordinates id):aux xs
        aux (x:xs) = aux xs
