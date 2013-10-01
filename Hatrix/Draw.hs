{-# LANGUAGE NamedFieldPuns     #-}

module Draw(
    Draw(), draw, write
    ) where 


import UI.HSCurses.Curses(mvWAddStr, stdScr)
import Datatypes


write :: [Char] -> Coord -> IO ()
write str (Coord x y) = mvWAddStr stdScr x y str


class Draw a where
    draw :: a -> IO ()


instance Draw Snake where
    draw Snake{parts} = 
        mapM_ drawSnakeBody parts
        where
            drawSnakeBody SnakeBody{coord, symbol} =
                write [symbol] coord