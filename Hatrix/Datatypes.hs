{-# LANGUAGE NamedFieldPuns     #-}

module Datatypes(
    Coord(..),
    Snake(..),
    HState(..),
    Randomizator(..),
    SnakeBody(..),

--  snake methods
    isVisible,
    moveForward,
    createTail,
    getNewSnake,
    getColSnakes
    ) where

import Control.Applicative

data Randomizator = Randomizator {  getSomeChar       :: IO Char
                                  , getSomeBool       :: IO Bool
                                  , getSomeSnakeSize  :: IO Int
                                  , getRandomColCoord :: Int -> Int -> IO Coord
                                 }


data Coord = Coord {y :: Int, x :: Int} deriving (Show, Eq)
forward' :: Coord -> Coord
forward' c@Coord{y} = c{y=y+1}

back' :: Coord -> Coord
back' c@Coord{y} = c{y=y-1}

createTail :: Coord -> -- starting coordinate
              Int ->   -- tail length
              [Coord]
createTail startPoint tailLength = createTail' tailLength [startPoint]
    where
        createTail' :: Int -> [Coord] -> [Coord]
        createTail' count coords
            | count <= 0 = coords
            | otherwise = createTail' (count-1) (coords ++ [back' $ last coords])


data SnakeBody = SnakeBody { coord    :: Coord
                            , symbol  :: Char
                           } deriving (Eq, Show)   


data Snake = Snake { parts :: [SnakeBody] } deriving(Show, Eq)


isVisible :: (Int, Int) -> Snake -> Bool
isVisible (winH, winW) Snake{parts} = any inWindow coords
    where 
        coords = map (\SnakeBody{coord} -> coord) parts
        inWindow Coord{y, x} = y < winH && x < winW


moveForward :: Snake -> Snake
moveForward Snake{parts}= Snake $ map forwardPart parts
    where forwardPart sb@SnakeBody{coord} = sb{coord=forward' coord}


getColSnakes :: Int -> [Snake] -> [Snake]
getColSnakes y' snakes= filter hasY snakes
  where 
    hasY Snake{parts} = any hasY' parts
    hasY' SnakeBody{coord} = y coord == y'


getNewSnake :: Randomizator ->   -- state
               Coord  ->   -- Starting Point
               Int    ->   -- Snake length
               IO Snake
getNewSnake randomizer coord len = Snake <$> createParts
    where
        newCoords = coord: createTail coord len

        createParts :: IO [SnakeBody]
        createParts = mapM createPart newCoords

        createPart :: Coord -> IO SnakeBody
        createPart coord' = do
            ch <- getSomeChar randomizer
            return $ SnakeBody coord' ch


data HState = HState {  snakes         :: [Snake]
                      , randomizer     :: Randomizator
                      , winSize        :: (Int, Int) 
                      , snakesCount    :: Int
                      , snakeLen       :: Int
                      }