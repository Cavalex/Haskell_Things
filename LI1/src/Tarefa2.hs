{- |

= Introdução 

Na tarefa 2 foi onde passamos mais tempo e onde pela primeira vez conseguimos que desse realmente para jogar (apesar de todos os comandos que eram precisos
para isso). Sendo que esse era o grande objetivo da tarefa.

= Objetivos 

Nesta tarefa o principal seria fazer uma função que nos permitisse jogar. Logo, teríamos de seguir todas as regras do jogo, O nosso maior objetivo foi 
fazer com que tudo estivesse dentro das diretrizes. Isso implicaria que todos os jogadores pudessem ter o comportamento cero em relação ao ambiente que 
os rodeia e vice-versa.

= Discussão e conclusão

Gostamos bastante de realizar esta tarefa, que apesar de muito morosa, obtivemos os resultados pretendidos e conseguimos jogar realmente, o que nos deu 
uma motivação extra para o resto do trabalho

-}

module Tarefa2 where

import Types
import Tarefa1

-- * Funções teste

-- | 'State' teste
s1 :: State
s1 = createInitialState js enemies (writePieceOn (10,2) (Food Big) m1)

-- | Lista de inimigos teste
enemies :: [Player]
enemies = [enemy1, enemy2]

-- | Lista de jogadores teste
js :: [Player]
--js = [jogador1, jogador2]
js = [jogador1]

-- | Inimigo teste
enemy1 :: Player
enemy1 = Ghost (GhoState (2,(3,10),2,R,0,1) Alive)

-- | Inimigo teste
enemy2 :: Player
enemy2 = Ghost (GhoState (3,(3,11),2,D,0,1) Alive)

-- | Jogador teste
jogador1 :: Player
jogador1 = Pacman (PacState (0,(7,11),2,R,5,1) 0 Open Normal)

-- | Jogador teste
jogador2 :: Player
jogador2 = Pacman (PacState (1,(2,10),2,R,10,1) 0 Open Normal)

-- | Cria um estado inicial, função para testes
createInitialState :: [Player] -- ^ A lista de 'Player's que queremos que façam parte do estado
                   -> [Player] -- ^ Outra lista de 'Player's
                   -> Maze -- ^ O 'Maze' que passamos ao 'State'
                   -> State -- ^ O novo 'State'
createInitialState ps es m = (State  m (ps++es) 1)

-- * Funções essenciais e auxiliares

--Para o movimento temos de encontrar uma função que adicione ou retire valores ao tipo Coords dependendo se vai para (R,L) ou (U,D) respetivamente
-- | Muda as coordenadas do jogador dada uma lista de coordenadas
updatePos :: Maze          -- ^ O 'Maze' onde se encontra o jogador
          -> Coords        -- ^ As 'Coords' atuais
          -> [Orientation] -- ^ A lista de 'Orientation's
          -> Coords        -- ^ As novas 'Coords'
updatePos m (x,y) [] = (x,y)
updatePos (cr:crs) (x,y) (c:xs) =
    --condicoes para se mover
    case c of R -> if y == (length cr)-1 && (case checkPiece (x,(length cr)-1) (cr:crs) of Empty -> True) then updatePos (cr:crs) (x,0) xs else updatePos (cr:crs) (x,(y+1)) xs
              L -> if y == 0 && (case checkPiece (x,0) (cr:crs) of Empty -> True) then updatePos (cr:crs) (x,(length cr-1)) xs else updatePos (cr:crs) (x,(y-1)) xs
              U -> updatePos (cr:crs) ((x-1),y) xs
              D -> updatePos (cr:crs) ((x+1),y) xs
              _ -> error "Sem Orientacao"

-- | Escreve uma 'Piece' 'Empty' nas coordenadas passadas no 'Maze' passado
updateMaze :: Coords -- ^ As 'Coords' onde queremos escrever
           -> Maze   -- ^ O 'Maze' antigo
           -> Maze   -- ^ O 'Maze' novo
updateMaze (a,b) m = writePieceOn (b+1,a+1) Empty m

-- | Retorna a peça numas certas 'Coords' dum certo 'Maze'
checkPiece :: Coords -- ^ As 'Coords' que queremos ver
           -> Maze   -- ^ O 'Maze' onde vamos ver a 'Piece'
           -> Piece  -- ^ A 'Piece' nas 'Coords' passadas do 'Maze' de cima
checkPiece (x,y) (c:cs) = if x == 0 then checkCorridor y c else checkPiece (x-1,y) cs

-- | Função auxiliar à 'checkPiece', vê a peça num determinado 'Corridor'
checkCorridor :: Int      -- ^ A ordenada do 'Corridor'
              -> Corridor -- ^ O 'Corridor' do 'Maze' passado na 'checkPiece'
              -> Piece    -- ^ A 'Piece' na respetiva ordenada
checkCorridor _ [] = error "CheckCorridor que pertence à checkPiece recebeu um array vazio"
checkCorridor x (p:ps) = if x == 0 then p else checkCorridor (x-1) ps

-- | Retorna True se as coordenadas passadas são acessíveis, False caso contrário.
canMove :: Coords -- ^ As 'Coords' que queremos ver
        -> Maze   -- ^ O 'Maze' onde vamos passar as 'Coords'
        -> Bool   -- ^ O 'Bool' que a função retorna
canMove c m = aux cp
    where
        cp = checkPiece c m
        aux Wall = False
        aux (PacPlayer (Ghost (GhoState (a,b,c,d,e,f) Alive))) = False
        aux (PacPlayer (Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc mode))) = False
        aux _ = True

-- | Muda o 'GhostMode' de todos os fantasmas para Dead
killGhosts :: [Player] -- ^ A lista de jogadores onde queremos "matar" os fantasmas
           -> [Player] -- ^ A nova lista de jogadores com os fantasmas "mortos"
killGhosts [] = []
killGhosts ((Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)):xs) = (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) Dead)):killGhosts xs
killGhosts (x:xs) = x:killGhosts xs

-- | Muda o 'GhostMode' de todos os fantasmas para Alive
reviveGhosts :: [Player] -- ^ A lista de jogadores onde queremos "reviver" os fantasmas
             -> [Player] -- ^ A nova lista de jogadores com os fantasmas "vivos"
reviveGhosts [] = []
reviveGhosts ((Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)):xs) = (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) Alive)):reviveGhosts xs
reviveGhosts (x:xs) = x:reviveGhosts xs

-- | Muda o 'PacMode' de um certo jogador para Dying
killPlayer :: [Player] -- ^ A lista de 'Player's onde queremos "matar" o 'Player'
           -> Coords -- ^ As 'Coords' do 'Player' a matar
           -> [Player] -- ^ A nova lista de 'Player's
killPlayer [] _ = []
killPlayer (x:xs) (a,b) = 
    case x of (Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc mode)) -> if a == xc && b == yc then (Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc Dying)):xs else killPlayer xs (a,b)
              _ -> killPlayer xs (a,b)

-- | Retorna a 'Orientation' de um 'Player'
getOrientation :: Player -- ^ O 'Player' a que queremos retirar a 'Orientation'
               -> Orientation -- ^ A 'Orientation' do 'Player'
getOrientation (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) = o
getOrientation (Pacman (PacState (ip,(xc,yc),i1,o,i2,i3) tm oc mode)) = o


-- | Muda a 'Orientation' de um 'Player'
setOrientation :: Player -- ^ O 'Player' a que queremos mudar a 'Orientation'
               -> Orientation -- ^ A nova 'Orientation' do 'Player'
               -> Player -- ^ O novo 'Player'
setOrientation (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) or = (Ghost (GhoState (ip,(xe,ye),i1,or,s,l) gMode))
setOrientation (Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)) or = (Pacman (PacState (ip,(xc,yc),i1,or,i2,i3) tm oc mode))

-- | Retorna o ID de um 'Player'
getID :: Player -- ^ O 'Player' a que queremos retirar o ID
      -> Int    -- ^ O ID do 'Player'
getID (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) = ip
getID (Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)) = ip

-- | Retorn as 'Coords' de um 'Player'
getCoords :: Player -- ^ O 'Player' a que queremos retirar as 'Coords'
          -> Coords -- ^ As 'Coords' do 'Player'
getCoords (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) = (xe,ye)
getCoords (Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)) = (xc,yc)

-- | Muda as 'Coords' de um 'Player'
changeCoords :: Coords -- ^ As novas 'Coords'
             -> Player -- O 'Player' antigo
             -> Player -- O novo 'Player'
changeCoords (x,y) (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) = (Ghost (GhoState (ip,(x,y),i1,o,s,l) gMode))
changeCoords (x,y) (Pacman (PacState (ip,(xc,yc),i1,op,i2,i3) tm oc mode)) = (Pacman (PacState (ip,(x,y),i1,op,i2,i3) tm oc mode))

-- | Retorna o 'Ghost' que se encontra nas dadas coordenadas, para uma lista de jogadores.
getEnemyOnCoords :: [Player]      -- ^ A lista de 'Player's
                 -> Coords        -- ^ As 'Coords' onde queremos detetar o 'Ghost'
                 -> (Player, Int) -- ^ Um tuple com o 'Ghost' e o seu ID
getEnemyOnCoords [] _ = error "Nao há nenhum inimigo lá"
getEnemyOnCoords (x:xs) (a,b) = 
    case x of (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) -> if a == xe && b == ye then ((Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)), ip) else getEnemyOnCoords xs (a,b)
              _ -> getEnemyOnCoords xs (a,b)

-- | Revive um 'Ghost' e manda-o para umas certas 'Coords'
updateState :: Coords -- ^ As novas 'Coords' 
            -> Player -- ^ O 'Player' antigo (parte-se do princípio que é um 'Ghost')
            -> State  -- O 'State' antigo
            -> State  -- O novo 'State' com o inimigo atualizado
updateState (xc,yc) p (State m (x:xs) l)
    | getID p == getID x = (State m newPlayers l)
    | otherwise = updateState (xc,yc) p (State m (xs++[x]) l)
    where
        newPlayers = xs ++ reviveGhosts [(changeCoords (xc,yc) p)]

-- | Chama a 'updateState' ao encontrar o inimigo presente nas coordenadas passadas 
respawnEnemies :: [Coords] -- ^ A lista de 'Coords' do centro da casa 
               -> Coords   -- ^ As 'Coords' onde se encontra o inimigo
               -> State    -- O 'State' antigo
               -> State    -- O novo 'State'
respawnEnemies (x:xs) (a,b) (State maze (p:ps) l)
    | getID p == snd (getEnemyOnCoords (p:ps) (a,b)) = updateState x p  (State maze (organizePlayers (p:ps)) l)
    | otherwise = respawnEnemies (x:xs) (a,b) state
    where
        state = (State maze (ps++[p]) l)

-- | Retorna uma lista de 'Coord's entre dois pontos que se encontram na mesma linha horizontal
getCoordsOnLine :: Coords   -- ^ O primeiro ponto
                -> Coords   -- ^ O segundo ponto
                -> [Coords] -- ^ A lista de 'Coords' retornada
getCoordsOnLine (y1,x1) (y2,x2)
    | y1 /= y2 = error $ "y's diferentes na função getCoordsOnLine: " ++ show y1 ++ " e " ++ show y2
    | x1 == x2 = [(y1,x1)]
    | otherwise = (y1,x1):getCoordsOnLine (y1,x1+1) (y2,x2)

-- | Este vai retornar as 'Coords' onde se encontra a casa
getHouseCoords :: Maze     -- ^ O maze sobre o qual queremos encontrar as 'Coords' da casa 
               -> [Coords] -- ^ A lista de 'Coords' da casa
getHouseCoords (x:xs)
    | lx `mod` 2 /= 0 = getCoordsOnLine (fstY-1, xe1+2) (fstY-1, xe1+6)
    | otherwise = getCoordsOnLine (fstY-1, xe1+3) (fstY-1, xe2+5)
    where
        lx = length x
        ly = length (x:xs)
        xe1 = ((lx-7) `div` 2) + 1
        xe2 = ((lx-6) `div` 2) + 1
        fstY = 
            if ly `mod` 2 == 0
                then (ly-2) `div` 2 + 1
                else (ly-1) `div` 2 + 1

-- | Organiza a lista dos jogadores por ordem crescente
organizePlayers :: [Player] -- ^ Recebe uma lista desorganizada de 'Player's
                -> [Player] -- ^ Retorna a lista organizada de 'Player's
organizePlayers [] = []
organizePlayers [x] = [x]
organizePlayers (x:xs)
    | getID x < getID y = x : organizePlayers xs
    | otherwise = y : organizePlayers (x:ys)
    where (y:ys) = organizePlayers xs

-- | Verifica se há algum inimigo "vivo" numas certas 'Coords'
isLiveEnemyOnCoords :: [Player] -- ^ Recebe uma lista de 'Player's que contém os inimigos
                    -> Coords   -- ^ Recebe umas certas 'Coords'
                    -> Bool     -- ^ Retorna True caso o 'GhostMode' seja Dead, False caso contrário
isLiveEnemyOnCoords [] _ = False
isLiveEnemyOnCoords (x:xs) (a,b) =
    case x of (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) Alive)) -> if a == xe && b == ye then True else isLiveEnemyOnCoords xs (a,b)
              _ -> isLiveEnemyOnCoords xs (a,b)

-- | Verifica se há algum inimigo "morto" numas certas 'Coords'
isDeadEnemyOnCoords :: [Player] -- ^ Recebe uma lista de 'Player's que contém os inimigos
                    -> Coords   -- ^ Recebe umas certas 'Coords'
                    -> Bool     -- ^ Retorna True caso o 'GhostMode' seja Alive, False caso contrário
isDeadEnemyOnCoords [] _ = False
isDeadEnemyOnCoords (x:xs) (a,b) = 
    case x of (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) Dead)) -> if a == xe && b == ye then True else isDeadEnemyOnCoords xs (a,b)
              _ -> isDeadEnemyOnCoords xs (a,b)

-- | Verifica se há algum 'Player' "vivo" numas certas 'Coords'
isLivePlayerOnCoords :: [Player] -- ^ Recebe uma lista de 'Player's
                     -> Coords   -- ^ Recebe umas certas 'Coords'
                     -> Bool     -- ^ Retorna True caso o 'PacMode' seja 'Normal', False caso contrário
isLivePlayerOnCoords [] _ = False
isLivePlayerOnCoords (x:xs) (a,b) = 
    case x of (Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc Normal)) -> if a == xc && b == yc then True else isLivePlayerOnCoords xs (a,b)
              _ -> isLivePlayerOnCoords xs (a,b)

-- | Verifica se há algum 'Player' que possa comer o 'Ghost' numas certas 'Coords'
isNotEatableEnemyOnCoords :: [Player] -- ^ Recebe uma lista de 'Player's
                          -> Coords   -- ^ Recebe umas certas 'Coords'
                          -> Bool     -- ^ Retorna True caso o 'PacMode' seja Mega ou Dying, False caso contrário
isNotEatableEnemyOnCoords [] _ = False
isNotEatableEnemyOnCoords (x:xs) (a,b) = 
    case x of (Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc Mega)) -> if a == xc && b == yc then True else isNotEatableEnemyOnCoords xs (a,b)
              (Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc Dying)) -> if a == xc && b == yc then True else isNotEatableEnemyOnCoords xs (a,b)
              _ -> isNotEatableEnemyOnCoords xs (a,b)

-- | Atualiza um 'State' de acordo com um 'Play'
play :: Play -- ^ O 'Play' que vai atualizar o 'State'
     -> State -- ^ O 'State' antigo
     -> State -- ^ O novo 'State'
play _ (State m [] l) = error "Error! Player not found!"
play (Move i o) (State m ((Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc mode)):xs) l)
    | i == ip && isThereAliveEnemy = newStateEnemy
    | i == ip && cMove = newStateCanMove
    | i == ip && cMove == False = newStateCantMove
    | otherwise = play (Move i o) (State m (xs++[x]) l)
    where
        newMouth = if (oc == Open) then Closed else Open
        cMove = canMove npCo m
        newStateCanMove
            | isThereDeadEnemy = respawnEnemies (getHouseCoords m) npCo (State newMaze (organizePlayers (newPlayer:newPlayerList)) l)
            | otherwise = (State newMaze (organizePlayers (newPlayer:newPlayerList)) l)
        newStateEnemy = (State m (organizePlayers (oldPlayer:newPlayerList)) l)
        newStateCantMove = (State m (organizePlayers (oldPlayer:newPlayerList)) l)
        newMaze = updateMaze pCo m
        newPlayer = Pacman (PacState (ip,npCo,vel,o,newScore,newLives) newTimeMega newMouth newPacMode)
        oldPlayer = Pacman (PacState (ip,pCo,vel,o,newScore,newLives) newTimeMega oc newPacMode)
        npCo = if (o == op) then updatePos m (xc,yc) [o] else pCo
        x = Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc mode)
        pCo = (xc, yc)
        nextPiece = checkPiece npCo m
        isThereAliveEnemy = isLiveEnemyOnCoords xs npCo
        isThereDeadEnemy = isDeadEnemyOnCoords xs npCo
        newPacMode = if (mode == Mega && timeMega > 0) then Mega else
            case nextPiece of 
                Food Big -> Mega
                PacPlayer (Ghost (GhoState (ip,(xe,ye),i1,o,s,l) gMode)) ->
                    case gMode of 
                        Alive -> Dying
                        _ -> Normal
                _ -> Normal
        newPlayerList = 
            case newPacMode of
                Mega -> if mode == Normal then killGhosts xs else xs
                -- Não tenho a certeza se é suposto os fantasmas ficarem sempre mortos no caso
                -- de um jogador começar vivo mas no mapa está um fantasma morto. Neste caso,
                -- ele vai ficar sempre morto excepto quando o jogador entra e sai de Mega.
                --Normal -> if mode == Mega then reviveGhosts xs else xs
                Normal -> reviveGhosts xs
                _ -> xs
        newLives = if isThereAliveEnemy then lives - 1 else lives
        newTimeMega = 
            case nextPiece of
                Food Big -> timeMega + 7
                _ -> if timeMega == 0 then timeMega else timeMega - 1
        newScore = if o == op then
            (case nextPiece of
                Food Little -> score + 1
                Food Big -> score + 5
                _ -> score) + (if isThereDeadEnemy then 10 else 0)
                else score

play (Move i o) (State m ((Ghost (GhoState (ip,(xc,yc),vel,op,score,lives) gMode)):xs) l) -- = play (Move i o) (State m (xs++[x]) l)
    | i == ip && isLivePlayerOnCoords xs ngCo = newStateCantMove
    | i == ip && cMove = newStateCanMove
    | i == ip && cMove == False = newStateCantMove
    | otherwise = play (Move i o) (State m (xs++[x]) l)
    where
        x = (Ghost (GhoState (ip,(xc,yc),vel,op,score,lives) gMode))
        newGhost = (Ghost (GhoState (ip,ngCo,newVel,o,score,lives) gMode))
        oldGhost = (Ghost (GhoState (ip,gCo,newVel,o,score,lives) gMode))
        gCo = (xc,yc)
        ngCo = if (o == op) then updatePos m (xc,yc) [o] else gCo
        newVel = if (gMode == Dead) then 0.5 else 1
        newStateCanMove = (State m (organizePlayers (newGhost:newPlayerList)) l)
        newStateCantMove
            | (isLivePlayerOnCoords xs ngCo) && gMode == Alive = (State m (organizePlayers (oldGhost:(killPlayer xs ngCo))) l)
            | (isLivePlayerOnCoords xs ngCo) && gMode == Dead = respawnEnemies (getHouseCoords m) gCo (State m (organizePlayers (oldGhost:newPlayerList)) l)
            | otherwise = (State m (organizePlayers (oldGhost:newPlayerList)) l)
        cMove = canMove ngCo m
        newPlayerList = xs
        --ghostCanMove =
        --  case nextPiece of
        --       Wall -> False
        --       _ -> True
