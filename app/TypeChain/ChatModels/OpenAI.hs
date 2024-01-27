{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeChain.ChatModels.OpenAI (OpenAIChat(..), OpenAIChatModel(..), mkOpenAIChat) where

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

--instance ToJSON GPT35Turbo where 
--    toJSON (GPT35Turbo k msgs temp) = object [ "model"       .= ("gpt-3.5-turbo" :: String)
--                                             , "temperature" .= temp
--                                             , "messages"    .= msgs
--                                             ]

data OpenAIChatModel = GPT35Turbo | GPT4 | GPT4Turbo

instance Show OpenAIChatModel where 
    show GPT35Turbo = "gpt-3.5-turbo"
    show GPT4       = "gpt-4"
    show GPT4Turbo  = "gpt-4-turbo-preview"

instance ToJSON OpenAIChatModel where 
    toJSON = toJSON . show

data OpenAIChat = OpenAIChat { chatModel   :: OpenAIChatModel
                             , messages    :: [Message]
                             , temperature :: Float
                             , apiKey      :: ApiKey
                             } deriving Generic

instance ToJSON OpenAIChat where 
    toJSON (OpenAIChat m msgs t k) = object [ "model"       .= m
                                            , "temperature" .= t
                                            , "messages"    .= msgs
                                            ]

---- | A chat model that uses OpenAI's GPT-3.5-Turbo model
--data GPT35Turbo = GPT35Turbo { key         :: ApiKey 
--                             , messages    :: [Message]
--                             , temperature :: Float
--                             }

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
mkOpenAIChat :: OpenAIChatModel -> ApiKey -> [Message] -> OpenAIChat
mkOpenAIChat model k messages = OpenAIChat model messages 0.7 k

mkOpenAIChatHeaders :: ApiKey -> RequestHeaders
mkOpenAIChatHeaders k = [("Content-Type", "application/json"), ("Authorization", "Bearer " <> k)]

mkGPT35TurboRequest :: MonadThrow m => OpenAIChat -> m Request
mkGPT35TurboRequest gpt = do 
    initReq <- parseRequest "https://api.openai.com/v1/chat/completions"
    return $ initReq { requestHeaders = mkOpenAIChatHeaders (apiKey gpt) 
                     , requestBody    = RequestBodyLBS (encode gpt)
                     , method         = "POST"
                     }

instance ChatModel OpenAIChat where 
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
