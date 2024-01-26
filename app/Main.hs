{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens

import Minichain.ChatModels.Types 
import Minichain.ChatModels.OpenAI

import DotEnv

toUserMessage :: Message -> Message 
toUserMessage = role .~ User

-- Talk between two different gpt-3.5-turbo instances (_1 and _2 are lenses from Control.Lens)
convo :: Minichain (GPT35Turbo, GPT35Turbo) ()
convo = do 
    r1 <- _1 `predicts` "Hello, how are you?" -- Asks 1 from 2 (fake msg to start convo)
    r2 <- _2 `predictsMsgs` map toUserMessage r1 -- Feed 1's response to 2
    r3 <- _1 `predictsMsgs` map toUserMessage r2 -- Feed 2's response to 1 
    r4 <- _2 `predictsMsgs` map toUserMessage r3 -- Feed 1's response to 2
    pure () -- Do Nothing: End of convo

main :: IO ()
main = do 
    env <- loadEnv DefaultEnv
    let Just key = env `getEnv` "OPENAI_API_KEY"
        gpt1 = mkGPT35Turbo key [] 
        gpt2 = mkGPT35Turbo key []

    (gpt1', gpt2') <- execStateT convo (gpt1, gpt2) -- Perform convo with models

    mapM_ print (messages gpt1') >> putStrLn "-------------------" 
    mapM_ print (messages gpt2')
