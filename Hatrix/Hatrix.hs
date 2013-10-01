{-# LANGUAGE NamedFieldPuns     #-}

module Hatrix(
    mainLoop
    ) where

import Control.Concurrent(threadDelay)
import System.IO
import Control.Applicative
import UI.HSCurses.Curses
import Draw(draw)
import Datatypes


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


getNewSnake :: HState ->   -- state
               Coord  ->   -- Starting Point
               Int    ->   -- Snake length
               IO Snake
getNewSnake st@HState{randomizer} coord len = Snake <$> createParts
    where
        newCoords = (coord:createTail coord len)
        createParts :: IO [SnakeBody]
        createParts = mapM createPart newCoords
        createPart :: Coord -> IO SnakeBody
        createPart coord = do
            ch <- getSomeChar randomizer
            return $ SnakeBody coord ch



mainLoopStart :: HState -> IO (Either String HState)
mainLoopStart state@HState{snakes} = do
    wclear stdScr
    draw $ snakes !! 0
    refresh
    threadDelay 200000
    key <- keyListen
    case key of
        Just 'z'  -> return $ Left "bye - bye"
        _ -> return $ Right state{snakes=processSnakes state snakes}


mainLoop :: HState -> IO String
mainLoop ml = mainLoopStart ml >>= either return mainLoop
