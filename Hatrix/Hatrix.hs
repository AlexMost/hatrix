{-# LANGUAGE NamedFieldPuns     #-}

module Hatrix(
    mainLoop
    ) where

import Datatypes(HState(..))
import Control.Concurrent(threadDelay)
import System.IO
import Control.Applicative


keyListen :: IO (Maybe Char)
keyListen = do
    result <- hReady stdin
    if result 
        then (Just) <$> getChar 
        else return Nothing


mainLoopStart :: HState -> IO (Either String HState)
mainLoopStart state@HState{snakes} = do
    threadDelay 200000
    key <- keyListen
    case key of
        Just 'z'  -> return $ Left "bye - bye"
        _ -> return $ Right state


mainLoop :: HState -> IO String
mainLoop ml = mainLoopStart ml >>= either return mainLoop
