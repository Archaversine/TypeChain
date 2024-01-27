{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeChain.ChatModels.OpenAI (GPT35Turbo(..), mkGPT35Turbo) where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Functor (($>))

import GHC.Generics (Generic)

import TypeChain.ChatModels.Types

import Network.HTTP.Simple
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy as BS

instance ToJSON GPT35Turbo where 
    toJSON (GPT35Turbo k msgs temp) = object [ "model"       .= ("gpt-3.5-turbo" :: String)
                                             , "temperature" .= temp
                                             , "messages"    .= msgs
                                             ]

-- | A chat model that uses OpenAI's GPT-3.5-Turbo model
data GPT35Turbo = GPT35Turbo { key         :: ApiKey 
                             , messages    :: [Message]
                             , temperature :: Float
                             }

-- | A list of responses from OpenAI's GPT-3.5-Turbo model
data Choices = Choices { message       :: Message 
                       , finish_reason :: String 
                       , index         :: Int
                       } deriving Generic

instance FromJSON Choices 

-- | Minimal JSON response datatype from OpenAI's GPT-3.5-Turbo model
data OpenAIResponse = OpenAIResponse { model   :: String
                                     , choices :: [Choices]
                                     } deriving Generic


instance FromJSON OpenAIResponse where 
    parseJSON = withObject "OpenAIResponse" $ \o -> do 
        model   <- o .: "model"
        choices <- o .: "choices"
        return $ OpenAIResponse model choices

-- | Create a GPT35Turbo model with default values
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
    predicts model m = do 
        let msgs = toMsgList m

        addMsgsTo model msgs

        gpt <- gets (view model)
        req <- mkGPT35TurboRequest gpt
        res <- httpLBS req

        case decode @OpenAIResponse (responseBody res) of 
            Nothing -> liftIO $ putStrLn "WARNING: Failed to decode OpenAIResponse" $> []
            Just r  -> let newMsgs = map message (choices r) in addMsgsTo model newMsgs $> newMsgs

    addMsgsTo model msgs = model %= \m -> m { messages = messages m <> toMsgList msgs }
