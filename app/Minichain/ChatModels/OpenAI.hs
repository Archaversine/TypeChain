{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minichain.ChatModels.OpenAI where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Aeson
import Data.ByteString.Lazy (ByteString)

import GHC.Generics (Generic)

import Minichain.ChatModels.Types

import Network.HTTP.Simple
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy as BS

instance ToJSON GPT35Turbo where 
    toJSON (GPT35Turbo k msgs temp) = object [ "model"       .= ("gpt-3.5-turbo" :: String)
                                             , "temperature" .= temp
                                             , "messages"    .= msgs
                                             ]

data GPT35Turbo = GPT35Turbo { key         :: ApiKey 
                             , messages    :: [Message]
                             , temperature :: Float
                             }

data Usage = Usage { prompt_tokens     :: Int 
                   , completion_tokens :: Int
                   , total_tokens      :: Int
                   } deriving Generic

data Choices = Choices { message       :: Message 
                       , finish_reason :: String 
                       , index         :: Int
                       } deriving Generic

data OpenAIResponse = OpenAIResponse { model   :: String
                                     , choices :: [Choices]
                                     } deriving Generic

instance FromJSON Usage 
instance FromJSON Choices 

instance FromJSON OpenAIResponse where 
    parseJSON = withObject "OpenAIResponse" $ \o -> do 
        model   <- o .: "model"
        choices <- o .: "choices"
        return $ OpenAIResponse model choices

mkGPT35Turbo :: ApiKey -> [Message] -> GPT35Turbo
mkGPT35Turbo k messages = GPT35Turbo k messages 0.9

mkGPT35TurboHeaders :: ApiKey -> RequestHeaders
mkGPT35TurboHeaders k = [("Content-Type", "application/json"), ("Authorization", "Bearer " <> k)]

mkGPT35TurboRequest :: MonadThrow m => GPT35Turbo -> m Request
mkGPT35TurboRequest gpt@(GPT35Turbo k msgs temp) = do 
    initReq <- parseRequest "https://api.openai.com/v1/chat/completions"
    return $ initReq { requestHeaders = mkGPT35TurboHeaders k
                     , requestBody    = RequestBodyLBS (encode gpt)
                     , method         = "POST"
                     }

instance ChatModel GPT35Turbo where 
    predictsMsgs model msgs = do 
        addMsgsTo model msgs

        gpt <- gets (view model)
        req <- mkGPT35TurboRequest gpt
        res <- httpLBS req

        case decode @OpenAIResponse (responseBody res) of 
            Nothing -> liftIO $ putStrLn "WARNING: Failed to decode OpenAIResponse" >> pure []
            Just r  -> let newMsgs = map message (choices r) in addMsgsTo model newMsgs >> pure newMsgs

    addMsgsTo model msgs = model %= \m -> m { messages = messages m <> msgs }