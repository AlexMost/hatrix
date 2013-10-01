import UI.HSCurses.Curses
import System.IO
import Datatypes
import Hatrix
import Randomizer


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  initCurses
  _ <- leaveOk True
  winSize' <- scrSize
  cursSet CursorInvisible
  echo False
  msg <- mainLoop $ (HState 
    [snake] (initRandomizator snakeChars snakeLen) winSize')
  putStrLn msg
  endWin
  where
    snake = Snake [(SnakeBody (Coord 2 2) 'z')]
    snakeChars = ['a', 'b', 'c', 'f']
    snakeLen = 7