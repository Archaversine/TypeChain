{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DotEnv

import TypeChain.ChatModels

main :: IO ()
main = do 
    env <- loadEnv DefaultEnv
    let Just key = env `getEnv` "OPENAI_API_KEY"
        model    = initOpenAIChat { chatModel = GPT35Turbo, apiKey = key }

    let prompt = "Hi!" :: String

    response <- evalStateT (predict prompt) model
    mapM_ print response
