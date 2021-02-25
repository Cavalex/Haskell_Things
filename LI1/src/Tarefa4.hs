{- |

= Introdução

A tarefa 4, foi a primeira tarefa da segunda fase do trabalho. Sendo que implementou bibliotecas novas e novos tipos de dados, para fazer o jogo
ser mais funcional, tanto para um humano, tanto como para a pŕopria máquina (como nas tarefas 5 e 6).

= Objetivos

O principal objetivo desta tarefa passava por calcular o efeito da passagem de um instante de tempo, ou seja, que alterações é que se iriam notar no "Maze"
e nos seus componentes, tinhamos também de ter especial atenção à criação de uma função que permitisse que todos os jogadores do "Maze" se movimentassem.
Com base nisto, criamos funções que nos auxiliassem a pegar nos elementos importantes sobre todos os players, fazendo assim no fim, que, mediante as suas
caracteristicas (velocidade e coordenadas) se movimentassem.

= Discussão e conclusão

Fizemos tudo dentro do proposto apesar de ao ínicio termos algumas dúvidas sobre os novos tipos de dados, habituamo-nos a eles e conseguimos finalizar a 
tarefa dentro do que esperavamos.

-}

module Tarefa4 where

import Types
import Tarefa2
import Tarefa5
import Tarefa6

defaultDelayTime = 250 -- 250 ms

-- | Retorna a 'Orientation' de um 'Player'
getCoords :: Player -- ^ O 'Player' a que queremos retirar a 'Orientation'
          -> Orientation -- ^ A 'Orientation' do 'Player'
getCoords (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) = o
getCoords (Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)) = op

-- | Retorna a velocidade de um 'Player'
getVel :: Player -- ^ O 'Player' a que queremos retirar a velocidade
       -> Double -- ^ A velocidade do 'Player'
getVel (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) = i1
getVel (Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)) = i1

-- | Cria um 'Play' a partir de um 'Player'
createPlay :: Player -- ^ O 'Player' que vai fornecer informação ao 'Play'
           -> Play -- ^ O 'Play' resultante
createPlay (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) = if o /= Null then (Move ip o) else (Move ip R)
createPlay (Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)) = if op /= Null then (Move ip op) else (Move ip R)

-- | Faz com que cada jogador mude o seu 'State' mediante a sua velocidade
playEachPlayer :: Int      -- ^ O 'step'
               -> [Player] -- ^ A lista de 'Player's
               -> State    -- ^ O 'State' inicial
               -> State    -- ^ O 'State' final
playEachPlayer _ [] state = state
playEachPlayer step (x:xs) state = aux2
    where
        (State maze playerList level) = state
        (y:ys) = playerList
        ghostVel = getGhostVel (x:xs)
        aux [] s = s
        aux (x:xs) s = (aux xs) $ play x s
        aux2
            | ghostVel == 1 = aux (ghostPlay state) $ aux (playerPlay state) state
            -- não sei mesmo como fazer este caso, o do v = 1.5
            | ghostVel == 1.5 = aux (ghostPlay state) $ aux (playerPlay state) state
            | ghostVel <= 0.5 = if step `rem` 3 == 0 then aux (ghostPlay state) $ aux (playerPlay state) state else aux (playerPlay state) state
            | ghostVel >= 2 = aux (ghostPlay state) $ aux (playerPlay state) $ aux (ghostPlay state) state

-- | Extrai a velocidade dos Ghosts de uma lista de 'Player's
getGhostVel :: [Player] -- ^ Lista de 'Player's
            -> Double   -- ^ Velocidade dos 'Ghosts'
getGhostVel [] = error "No ghosts found?!"
getGhostVel ((Ghost (GhoState (ip,(xe,ye),vel,o,s,l) gMode)):xs) = vel
getGhostVel (_:xs) = getGhostVel xs

-- | Aplica um 'Play' a todos os 'Player's
playerPlay :: State  -- ^ O 'State' que contém a playerList 
           -> [Play] -- ^ A lista de jogadas
playerPlay (State maze playerList level) = aux playerList
    where
        s = (State maze playerList level)
        aux [] = []
        aux ((Pacman (PacState (ip,(xc,yc),vel,op,i2,i3) tm oc mode)):xs) = let (Just p) = bot ip s in p:aux xs
        aux (_:xs) = aux xs

-- | Atualiza as velocidades de todos os 'Player's da lista
updateVels :: [Player] -- ^ A lista de 'Player's inicial
           -> Int      -- ^ O nível do 'Player' 
           -> [Player] -- ^ A lista de 'Player's final
updateVels [] _ = []
updateVels ((Pacman (PacState (ip,(xc,yc),vel,op,i2,i3) tm oc mode)):xs) level = (Pacman (PacState (ip,(xc,yc),1,op,i2,i3) tm oc mode)):updateVels xs level
updateVels ((Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)):xs) level
    | level >= 1 && level < 3 = (Ghost (GhoState (ip,(xe,ye),0.5,o,s,l) gMode)):updateVels xs level
    | level >= 3 && level < 5 = (Ghost (GhoState (ip,(xe,ye),1,o,s,l) gMode)):updateVels xs level
    | level >= 5 && level < 7 = (Ghost (GhoState (ip,(xe,ye),1.5,o,s,l) gMode)):updateVels xs level
    | level >= 7 && level < 10 = (Ghost (GhoState (ip,(xe,ye),2,o,s,l) gMode)):updateVels xs level
    | level >= 10 = (Ghost (GhoState (ip,(xe,ye),3,o,s,l) gMode)):updateVels xs level
    | otherwise = (Ghost (GhoState (ip,(xe,ye),1,o,s,l) gMode)):updateVels xs level

-- | Para um dado​ 'step' e 'State', altera o estado do jogo em uma iteração
passTime :: Int   -- ^ O 'step'  
         -> State -- ^ O 'State' inicial 
         -> State -- ^ O 'State' final
passTime n state = playEachPlayer n newPlayerList newState
    where
        (State maze playerList level) = state
        newPlayerList = updateVels playerList level
        newState = State maze newPlayerList level
