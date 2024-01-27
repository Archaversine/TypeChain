{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens

import TypeChain.ChatModels

import DotEnv
 
-- Helper function to turn assistant messages into user messages 
-- We do this so we don't confuse the model and make it think it's talking 
-- to itself
toUserMessage :: Message -> Message 
toUserMessage msg = msg { _role = User }

convo :: TypeChain (GPT35Turbo, GPT35Turbo) [Message]
convo = do 
    let prompt = "Why does 1 + 1 = 2?"

    -- Add appropriate context to model 2 so it thinks it asked this 
    -- question.
    addMsgsTo _2 [Message Assistant prompt]

    -- Ask model 1 why 1 + 1 = 2 
    -- We do this to start a conversation between the two models
    response1 <- _1 `predicts` "Why does 1 + 1 = 2?"

    -- Feed model 1's response into model2
    _2 `predictsMsgs` map toUserMessage response1

main :: IO ()
main = do 
    env <- loadEnv DefaultEnv 
    let Just key = env `getEnv` "OPENAI_API_KEY" 
        model1   = mkGPT35Turbo key []
        model2   = mkGPT35Turbo key []

    (_, model2) <- execStateT convo (model1, model2)

    mapM_ print (messages model2)
