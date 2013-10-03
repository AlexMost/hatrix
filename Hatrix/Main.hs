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
                        ((fst winSize'), 100)
                        snakesCount'
                        snakeLen')
  putStrLn msg
  endWin
  where
    snakeChars = ['a', 'b', 'c', 'f']
    snakeLen' = 15
    snakesCount' = 300
    randomizer' = initRandomizator snakeChars snakeLen'
