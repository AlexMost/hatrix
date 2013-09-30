import UI.HSCurses.Curses
import System.IO
import Datatypes
import Hatrix


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  initCurses
  _ <- leaveOk True
  echo False
  msg <- mainLoop $ HState [snake]
  putStrLn msg
  endWin
  where
    snake = Snake [(SnakeBody (Coord 1 1) 'z')]