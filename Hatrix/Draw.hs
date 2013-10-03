{-# LANGUAGE NamedFieldPuns     #-}

module Draw(
    Draw(), draw, write
    ) where 


import UI.HSCurses.Curses
import Datatypes
import Control.Exception


write :: [Char] -> Coord -> IO ()
write str (Coord x y) = do
    try $ mvWAddStr stdScr x y str :: IO (Either SomeException ())
    return ()

writeHead :: [Char] -> Coord -> IO ()
writeHead str (Coord x y) = do
    try $ mvWAddStr stdScr x y str :: IO (Either SomeException ())
    return ()


class Draw a where
    draw :: a -> IO ()


instance Draw Snake where
    draw Snake{parts=(h:otherParts)} = do
        attrSet attr0 (Pair 2)
        mapM_ drawSnakeBody otherParts
        attrSet attr0 (Pair 1)
        writeHead [(symbol h)] (coord h)
        where
            drawSnakeBody SnakeBody{coord, symbol} =
                write [symbol] coord

instance Draw HState where
    draw HState{snakes} = do
        mapM_ draw snakes
