{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeChain.ChatModels.Types ( TypeChain
                                  , TypeChainT
                                  , ApiKey 
                                  , Role(..)
                                  , Message(..), role, content
                                  , pattern UserMessage 
                                  , pattern AssistantMessage 
                                  , pattern SystemMessage
                                  , MsgList(..)
                                  , ChatModel(..) 
                                  , module Control.Monad.State
                                  ) where 
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Kind (Constraint)

import GHC.Generics (Generic)

type TypeChain model = StateT model IO
type TypeChainT = StateT

type ApiKey = ByteString

-- | Way of distinguising who said what in a conversation
data Role = User | Assistant | System

-- | A message with a role and content (lenses @role@ and @content@)
data Message = Message { _role    :: Role 
                       , _content :: String 
                       } deriving Generic

instance Show Role where 
    show = \case 
        User      -> "user"
        Assistant -> "assistant" 
        System    -> "system"

instance Show Message where 
    show (Message r c) = show r ++ ": " ++ c

makeLenses ''Message

-- | Pattern synonym for creating a @Message@ with @User@ role
pattern UserMessage :: String -> Message 
pattern UserMessage s = Message User s

-- | Pattern synonym for creating a @Message@ with @Assistant@ role
pattern AssistantMessage :: String -> Message 
pattern AssistantMessage s = Message Assistant s 

-- | Pattern synonym for creating a @Message@ with @System@ role
pattern SystemMessage :: String -> Message 
pattern SystemMessage s = Message System s

instance ToJSON Role where 
    toJSON = \case 
        User      -> String "user"
        Assistant -> String "assistant"
        System    -> String "system"

instance FromJSON Role where 
    parseJSON = withText "Role" $ \case 
        "user"      -> pure User
        "assistant" -> pure Assistant
        "system"    -> pure System
        _           -> fail "Invalid role"

instance ToJSON Message where 
    toJSON (Message r c) = object [ "role"    .= r 
                                  , "content" .= c
                                  ]

instance FromJSON Message where 
    parseJSON = withObject "Message" $ \o -> do 
        role    <- o .: "role"
        content <- o .: "content"
        return $ Message role content

-- | Helper typeclass to allow for multiple types to be passed to the  
-- @ChatModel@ functions. 
--
-- NOTE: If this is used with the @OverloadedStrings@ extension, you will need 
-- type annotations when using the @String@ instance.
class MsgList a where 

    -- | Convert to @a@ list of messages
    toMsgList :: a -> [Message]

instance MsgList String where 
    toMsgList = toMsgList . UserMessage

instance MsgList Message where 
    toMsgList = pure

instance MsgList [Message] where 
    toMsgList = id

-- | A class for Chat Models
-- In order to achieve compatibility with as many different kinds of LLMS as 
-- possible, the predict function is constrained to MonadIO so that it has the 
-- capability to either make an API call, run a local model, or any other action 
-- that may require IO.
--
-- Computations with a @ChatModel@ are expected to be run in a @StateT@ monad 
-- (see @TypeChain@ and @TypeChainT@ for specific types) so that the model can 
-- be updated with new messages and the output messages can be logged. 
--
-- Functions that operate in a context where multiple models are available 
-- (e.g. @predicts@ and @addMsgsTo@) use lenses to allow extraction and 
-- modification of the model without knowing the specific state type. 
--
-- Exmaple: If working with two models, you can use @(model1, model2)@ as the 
-- state type and pass the @_1@ and @_2@ lenses to @predicts@ and @addMsgsTo@
-- to specify which model to use in the function.
class ChatModel a where 

    -- | Predict for current and only model
    -- This function should prompt the model (either via API or locally), log 
    -- the input messages, log the output messages, and return the output messages. 
    predict :: (MonadIO m, MonadThrow m, MsgList msg) => msg -> StateT a m [Message]
    predict = predicts id

    -- | Predict for a specific model via lens
    -- This function should prompt the model (either via API or locally), log 
    -- the input messages, log the output messages, and return the output messages. 
    predicts :: (MonadIO m, MonadThrow m, MsgList msg) => Lens' s a -> msg -> StateT s m [Message]

    -- | Add Messages for current and only model
    -- This function should ONLY log messages, the actual LLM itself should not 
    -- be modified.
    addMsgs :: (Monad m, MsgList msg) => msg -> StateT a m ()
    addMsgs = addMsgsTo id

    -- | Add Messages for specific model
    -- This function should ONLY log messages, the actual LLM itself should not 
    -- be modified.
    addMsgsTo :: (Monad m, MsgList msg) => Lens' s a -> msg -> StateT s m ()
