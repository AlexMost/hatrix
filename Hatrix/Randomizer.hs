module Randomizer(
    initRandomizator
    ) where
    
import System.Random
import Datatypes
import Test.QuickCheck
import Test.QuickCheck.Gen


initRandomizator :: [Char] ->     -- symbols for randomizer
                    Int ->        -- max snake size
                    Randomizator
initRandomizator charSymbols snakeSize =
    (Randomizator 
        getSomeChar' getSomeBool' getSomeSnakeSize' getRandomColCoord')
    where
        getSomeChar' :: IO Char
        getSomeChar' = do
            gen <- newStdGen
            return $ unGen (elements charSymbols) gen 1

        getSomeBool' :: IO Bool
        getSomeBool' = do
            gen <- newStdGen
            return $ unGen (elements [True, False]) gen 1

        getSomeSnakeSize' :: IO Int
        getSomeSnakeSize' = do
            gen <- newStdGen
            return $ unGen (choose (2, snakeSize)) gen 1

        getRandomColCoord' x' maxY = do
            y' <- getY'
            return $ Coord y' x'
            where 
                getY' = do
                    gen <- newStdGen
                    return $ unGen (choose (0, maxY)) gen 1