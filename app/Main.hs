{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

import Data.List.Split (splitOn)

import TypeChain.ChatModels

import DotEnv

chatPrompt :: String -> [Message]
chatPrompt = $(makeTemplate [ system "You are a helpful assistant who generates comma separated lists. A user will pass in a category, and you should generate 5 objects in that category in a comma separated list. ONLY return a comma separated list, and nothing more."
                            , user "{text}"
                            ])

toCommaList :: [Message] -> [String]
toCommaList = concatMap (splitOn ", " . view content)

convo :: String -> TypeChain OpenAIChat [String]
convo prompt = toCommaList <$> predict (chatPrompt prompt)

main :: IO ()
main = do 
    env <- loadEnv DefaultEnv 
    let Just key = env `getEnv` "OPENAI_API_KEY" 
        model    = initOpenAIChat { chatModel = GPT35Turbo, apiKey = key }

    response <- evalStateT (convo "colors") model
    mapM_ putStrLn response
