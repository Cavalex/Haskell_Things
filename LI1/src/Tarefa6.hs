{- |

= Introdução 

Nesta tarefa o objetivo passava por implementar um bot que jogasse sozinho e que tivesse a melhor performance possível nesse mesmo requesito. 
Uma das tarefas mais desafiantes do trabalho, pois requereria que implementássemos uma inteligência artificial ao Pacman para que ele conseguisse 
jogar de uma forma parecida a que um humano jogasse.


= Objetivos

O nosso principal objetivo passava por criar um pacman que realizasse as escolhas mais logicamente possível tendo em conta o seu meio envolvente, 
assim ele analisaria o que o rodeava e tomava a melhor opção. Sendo que como meio envolvente consideramos os fantasmas e a comida. No caso dos fantasmas 
tivemos de considerar os dois possíveis modos do Pacman, o "Normal" e o "Mega", se o Pacman estivesse em modo "Normal" teria de fugir dos fantasmas e ao mesmo
tempo tentar apanhar o máximom de comidas possível, caso contrário andaria atrás deles. Faltava então saber como pôr em prática, decidimos então usar a fórmula 
da distância entre dois pontos para descobrir qual a menor distância entre o pacman e um inimigo, e conforme o seu modo, fugir ou perseguir.


= Discussão e Conclusão

Esta, tal como a anterior foi uma das mais complicadas, tendo esta ainda uma dificuldade acrescida devido a ter que realizar mais de uma ação e mais do
que uma decisão por jogada. Mas mais uma vez no final comportou-se mediante o expectável.

-}

module Tarefa6 where 

import Types
import System.Random
import Tarefa2
import Tarefa5

-- | Função que retorna um 'Maybe Play' de acordo com o id de um 'Player' e um 'State' passado
bot :: Int  -- ^ O Id
    -> State -- ^ O 'State' inicial
    -> Maybe Play -- ^ A tomada de decisão do 'Player' a que corresponde o id
bot id (State maze playerList level)
    | mode == Normal || mode == Dying = Just $ eatMode s houseCoordinates id
    | mode == Mega = Just $ megaMode s houseCoordinates id
    --  | mode == Dying = Just $ runningMode s playerCoords
    where
        houseCoordinates = getHouseCoords maze
        s = (State maze playerList level)
        mode = getPacModeByID playerList id
        aux (x:xs) = aux xs

-- | Retorna todas as 'Coords' onde se encontra 'Food Little' ou 'Food Big' de um 'Maze'
getFoodCoords :: Maze  -- ^ O 'Maze' inicial
              -> [Coords] -- ^ As 'Coords' da Comida
getFoodCoords (x:xs) = cleanCoords $ map (\c -> if checkPiece c (x:xs) `elem` [Food Little, Food Big] then c else (-1,-1)) $ getAllCoords (x:xs) firstC lastC firstC
    where
        lengthX = length (x:xs) - 2
        lengthY = length x - 1
        lastC = (lengthX, lengthY)
        firstC = (1,1)

-- | Retorna uma lista sem 'Coord's do tipo (-1,-1)
cleanCoords :: [Coords] -- ^ Uma lista de 'Coords'
            -> [Coords] -- ^ A lista de 'Coords' só com as necessárias
cleanCoords [] = []
cleanCoords (x:xs) = if areCoordsEqual x (-1,-1) then cleanCoords xs else x:cleanCoords xs

-- | Encontra todas as 'Coords' de um 'Maze'
getAllCoords :: Maze -- ^ Um 'Maze'
             -> Coords -- ^ As 'Coords' do canto superior esquero de um 'Maze' 
             -> Coords -- ^ As 'Coords' do canto inferior direito de um 'Maze'
             -> Coords -- ^ Um acumulador de 'Coords'
             -> [Coords] -- ^ A lista de todas as 'Coords'
getAllCoords [] _ _ ac = [ac]
getAllCoords ([]:ys) fc lc (a,b) = getAllCoords ys fc lc (a,b)
getAllCoords ((x:xs):ys) fc lc (a,b)
    | b == snd lc && a == fst lc = [(a,b)]
    | b /= snd lc = (a,b):getAllCoords ((xs):ys) fc lc (a,b+1)
    | b == snd lc = (a,b):getAllCoords ((xs):ys) fc lc (a+1,snd fc)
    | otherwise = error "????? erro na getAllCoords da Tarefa6"

-- | Retorna as 'Coords' da comida mais próxima às 'Coords' dadas
getClosestFood :: [Coords] -- ^ A lista de comidas
           -> Coords   -- ^ As 'Coords'
           -> Coords   -- ^ O acumulador das 'Coords'
           -> Coords   -- ^ As 'Coords' da comida mais próxima
getClosestFood [] _ acD = acD
getClosestFood (x:xs) co acD
    | d < dAc = getClosestFood xs co x
    | otherwise = getClosestFood xs co acD
    where
        d = distance x co
        dAc = distance acD co

-- | Retorna uma lista com todas as 'Coords' de todos os 'Ghosts' dum 'State'
getAllGhostsCoords :: State  -- ^ O 'State' onde está a lista dos fantasmas
                   -> [Coords] -- ^ As 'Coords' de todos os fantasmas 
getAllGhostsCoords (State maze playerList level) = aux playerList
    where
        aux [] = []
        aux ((Ghost (GhoState (ip,(xc,yc),vel,op,score,lives) gMode)):xs) = (xc,yc):aux xs
        aux (_:xs) = aux xs

-- | O modo usado quando está em 'Mega', geralmente a perseguir os fantasmas
megaMode :: State -- ^ Um 'State'
         -> [Coords] -- ^ As 'Coords' da casa dos fantasmas
         ->  Int -- ^ O Id 
         -> Play -- ^ O 'Play'
megaMode (State maze playerList level) houseCoords id = getNextPlay playerList 0 id maze ghostOrientation ghostCoords (getNeighbours maze houseCoords ghostCoords) (getClosestGhost playerList ghostCoords (-1) (1000,1000)) houseCoords
    where
        ghostCoords = getCoordsByID playerList id
        ghostOrientation = getOrientationByID playerList id

-- | O modo em que ele está à procura de comida e consecutivamente a fugir dos 'Ghosts'
eatMode :: State  -- ^ Um 'State'
        -> [Coords] -- ^ As 'Coords' da casa dos fantasmas
        ->  Int -- ^ O Id
        -> Play -- ^ O 'Play'
eatMode (State maze playerList level) houseCoords id = getNextPlay playerList chaseScatter id maze ghostOrientation playerCoords (getNeighbours maze houseCoords playerCoords) alvo houseCoords
    where
        chaseScatter = if runEat then 1 else 0
        runEat = distance cG playerCoords <= 6
        alvo = if runEat then (idG, cG) else (-1, closestFood)
        closestFood = getClosestFood (getFoodCoords maze) playerCoords (1000,1000)
        (idG, cG) = getClosestGhost playerList playerCoords (-1) (1000,1000)
        playerCoords = getCoordsByID playerList id
        ghostOrientation = getOrientationByID playerList id
