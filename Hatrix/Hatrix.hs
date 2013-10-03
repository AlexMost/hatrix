{-# LANGUAGE NamedFieldPuns     #-}

module Hatrix(
    mainLoop
    ) where

import Control.Concurrent(threadDelay)
import System.IO
import Control.Applicative
import UI.HSCurses.Curses
import Draw
import Datatypes
import System.Random.Shuffle
import System.Random


keyListen :: IO (Maybe Char)
keyListen = do
    result <- hReady stdin
    if result 
        then (Just) <$> getChar 
        else return Nothing


processSnakes :: HState -> [Snake] -> [Snake]
processSnakes HState{winSize} = 
    moveSnakesForward . removeInvisible
    where
        removeInvisible = filter (isVisible winSize)
        moveSnakesForward = map moveForward


appendSnakeToCol :: HState -> Int -> IO HState
appendSnakeToCol st@HState{snakes, randomizer, winSize, snakeLen, snakesCount} num = 
    if (winHeight - lastSnakePartY colSnakes) > snakeLen && (length snakes <= snakesCount) then
        do
        -- generating random snake data
        randSnakeCoord <- (getRandomColCoord randomizer) num (lastSnakePartY snakes)
        randSnakeLen <- (getSomeSnakeSize randomizer)
        snake <- getNewSnake randomizer randSnakeCoord randSnakeLen
        return st{snakes=(snake:snakes)}
    else
        do
        return st
    where 
        colSnakes = getColSnakes num snakes
        winHeight = fst winSize
        lastSnakePartY [] = 0
        lastSnakePartY s  = minimum $ tailY's s
        tailY's = map (\Snake{parts} -> y $ coord $ last parts)


appendSnakesToCols :: HState ->
                      IO HState  -- state with new snakes
appendSnakesToCols st@HState{winSize=(_, w)} = do
    gen <- newStdGen
    shuffledCols <- return $ shuffle' [0..(w- 1)] w gen
    appendSnakesToCols' st shuffledCols


appendSnakesToCols' :: HState ->
                       [Int] ->  -- shuffled list of cols
                       IO HState
appendSnakesToCols' st [] = return st
appendSnakesToCols' st (currentNum: colNums) = do
    newColState <- appendSnakeToCol st currentNum
    appendSnakesToCols' newColState colNums 


mainLoopStart :: HState -> IO (Either String HState)
mainLoopStart state@HState{snakes=snakes'} = do
    wclear stdScr
    draw state
    newState <- appendSnakesToCols processedState
    refresh
    threadDelay 10000
    key <- keyListen
    case key of
        Just 'z'  -> return $ Left "bye - bye"
        _ -> return $ Right newState
        where
            processedState = state{snakes=processSnakes state snakes'}


mainLoop :: HState -> IO String
mainLoop ml = mainLoopStart ml >>= either return mainLoop
