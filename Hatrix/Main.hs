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
                        [] 
                        randomizer'
                        (200, (snd winSize'))
                        snakesCount'
                        snakeLen')
  putStrLn msg
  endWin
  where
    snakeChars = ['a', 'b', 'c', 'f']
    snakeLen' = 15
    snakesCount' = 1000
    randomizer' = initRandomizator snakeChars snakeLen'
