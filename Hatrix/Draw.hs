{-# LANGUAGE NamedFieldPuns     #-}

module Draw(
    Draw(), draw, write
    ) where 


import UI.HSCurses.Curses(mvWAddStr, stdScr)
import Datatypes
import Control.Exception


write :: [Char] -> Coord -> IO ()
write str (Coord x y) = do
    try $ mvWAddStr stdScr x y str :: IO (Either SomeException ())
    return ()


class Draw a where
    draw :: a -> IO ()


instance Draw Snake where
    draw Snake{parts} = 
        mapM_ drawSnakeBody parts
        where
            drawSnakeBody SnakeBody{coord, symbol} =
                write [symbol] coord

instance Draw HState where
    draw HState{snakes} = do
        mapM_ draw snakes
        write statusMessage coord'
        where
            statusMessage = "Snakes - " ++ (show $ length snakes)
            coord' = Coord 2 10
