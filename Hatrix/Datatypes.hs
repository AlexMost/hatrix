module Datatypes(
    Coord(..),
    Snake(..),
    HState(..),
    Randomizator(..),
    SnakeBody(..)
    ) where


data Randomizator = Randomizator {
                                    getRandomChar      :: IO Char,
                                    getRandomBool      :: IO Bool,
                                    getRandomSnakeSize :: IO Int
                                 }


data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)


data SnakeBody = SnakeBody {
                            coord    :: Coord
                            ,symbol  :: Char
                           } deriving (Eq, Show)   


data Snake = Snake {
                    cords :: [SnakeBody]
                    } deriving(Show, Eq)


data HState = HState {
                      snakes :: [Snake]        
                     }