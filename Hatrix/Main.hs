import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Datatypes
import Hatrix
import Randomizer

main :: IO ()
main = do
  start
  startColor
  useDefaultColors
  initPair (Pair 2) green black
  initPair (Pair 1) white black
  _ <- leaveOk True
  winSize' <- scrSize
  cursSet CursorInvisible
  echo False
  msg <- mainLoop $ (HState 
                        [] 
                        randomizer'
                        ((fst winSize'), winWidth)
                        snakesCount'
                        snakeLen'
                        delay)
  putStrLn msg
  endWin
  where
    snakeChars = ['a', 'b', 'c', 'f']
    snakeLen' = 15
    snakesCount' = 250
    randomizer' = initRandomizator snakeChars snakeLen'
    delay = 90000
    winWidth = 150
