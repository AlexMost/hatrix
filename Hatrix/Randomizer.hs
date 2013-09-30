module Randomizer where
import System.Random

data Randomizator = Randomizator {
                                    getRandomChar :: IO Char
                                    getRandomBool :: IO Bool
                                 }


initRandomizator :: [Char] -> -- snake symbols
                    Randomizator
initRandomizator snakeSymbols =
    Randomizator getRandomChars
    where
        getRandomChars :: IO Char
        getRandomChars = do
            gen <- newStdGen
            return $ unGen (elements snakeSymbols)
