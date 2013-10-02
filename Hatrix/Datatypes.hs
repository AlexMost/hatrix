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
    getNewSnake
    ) where

import Control.Applicative

data Randomizator = Randomizator {  getSomeChar      :: IO Char
                                  , getSomeBool      :: IO Bool
                                  , getSomeSnakeSize :: IO Int
                                 }


data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)
forward' :: Coord -> Coord
forward' c@Coord{x} = c{x=x+1}

back' :: Coord -> Coord
back' c@Coord{x} = c{x=x-1}

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
        inWindow Coord{x,y} = x < winH && y < winW


moveForward :: Snake -> Snake
moveForward Snake{parts}= Snake $ map forwardPart parts
    where forwardPart sb@SnakeBody{coord} = sb{coord=forward' coord}


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
                      }