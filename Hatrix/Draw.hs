module Draw(
    Draw(), draw, write, writePoint
    ) where 


import UI.HSCurses.Curses(mvWAddStr, stdScr, refresh)
import Datatypes


write :: String -> Coord -> IO ()
write str (Coord x y) = mvWAddStr stdScr x y str


class Draw a where
    draw :: a -> IO ()