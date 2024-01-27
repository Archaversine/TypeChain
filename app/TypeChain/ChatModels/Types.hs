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

class MsgList a where 
    toMsgList :: a -> [Message]

instance MsgList String where 
    toMsgList = toMsgList . UserMessage

instance MsgList Message where 
    toMsgList = pure

instance MsgList [Message] where 
    toMsgList = id

-- | A class for Chat Models
class ChatModel a where 
    -- | Predict for current and only model
    predict :: (MonadIO m, MonadThrow m, MsgList msg) => msg -> StateT a m [Message]
    -- Implicit type signature: 
    predict = predicts id

    -- | Predict for a specific model via lens
    predicts :: (MonadIO m, MonadThrow m, MsgList msg) => Lens' s a -> msg -> StateT s m [Message]

    -- | Add Messages for current and only model
    addMsgs :: (Monad m, MsgList msg) => msg -> StateT a m ()
    addMsgs = addMsgsTo id

    -- | Add Messages for specific model
    addMsgsTo :: (Monad m, MsgList msg) => Lens' s a -> msg -> StateT s m ()
