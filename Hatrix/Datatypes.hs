{-# LANGUAGE NamedFieldPuns     #-}

module Datatypes(
    Coord(..),
    Snake(..),
    HState(..),
    Randomizator(..),
    SnakeBody(..),
    isVisible,
    moveForward
    ) where


data Randomizator = Randomizator {  getSomeChar      :: IO Char
                                  , getSomeBool      :: IO Bool
                                  , getSomeSnakeSize :: IO Int
                                 }


data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)
forward' :: Coord -> Coord
forward' c@Coord{x} = c{x=x+1}


data SnakeBody = SnakeBody { coord    :: Coord
                            , symbol  :: Char
                           } deriving (Eq, Show)   


data Snake = Snake { parts :: [SnakeBody] } deriving(Show, Eq)

isVisible :: (Int, Int) -> Snake -> Bool
isVisible (winH, winW) Snake{parts} = any inWindow coords
    where 
        coords = map (\SnakeBody{coord} -> coord) parts
        inWindow Coord{x,y} = y < winH && x < winW

moveForward :: Snake -> Snake
moveForward Snake{parts}= Snake $ map forwardPart parts
    where forwardPart sb@SnakeBody{coord} = sb{coord=forward' coord}


data HState = HState { snakes     :: [Snake]
                      ,randomizer :: Randomizator
                      ,winSize    :: (Int, Int) }