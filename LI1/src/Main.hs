module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6

data Manager = Manager
    {   
         state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    }

loadManager :: Manager
loadManager = ( Manager (State maze playerList level) 0 0 0 0 defaultDelayTime )
  where 
    --(State maze playerList level) = loadMaze "maps/1.txt"
    -- mapa para testar o pathfinding
    (State maze playerList level) = loadMaze "maps/5.txt"

sortThroughPlayers :: [Player] -> Int -> Key -> [Player]
sortThroughPlayers [] _ _ = error []
sortThroughPlayers (x:xs) pid key
    | getID x == pid = (changeByKey x key):xs
    | otherwise = x:(sortThroughPlayers xs pid key)
    where
        changeByKey y k
            | k == KeyUpArrow = setOrientation y U
            | k == KeyDownArrow = setOrientation y D
            | k == KeyLeftArrow = setOrientation y L
            | k == KeyRightArrow = setOrientation y R

changeOrientation :: Key -> Int -> State -> State
changeOrientation key pid (State m playerList l) = (State m (sortThroughPlayers playerList pid key) l)

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer key (Manager state pid step before delta delay) = (Manager (changeOrientation key pid state) pid step before delta delay)

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0
                      drawString $ show (state man)
                    render

currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager state pid step before delta delay) = (Manager state pid step now (delta+(now-before)) delay)

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager state pid step before delta delay) = (Manager state pid step now 0 delay) -- se calhar sem o delay

nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager state pid step before delta delay) = resetTimer now (Manager (passTime (step+1) state) pid (step+1) before delta delay)

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = let (State maze pl level) = s in do
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO currentTime
  updateScreen w color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

gameOver :: [Player]
         -> String -- ^ Mensagem de fim de jogo
gameOver [] = ""
gameOver ((Pacman (PacState (ip,(xc,yc),vel,op,score,lives) timeMega oc mode)):xs) = 
    do if lives <= 0 then "GAME OVER" else ""

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

