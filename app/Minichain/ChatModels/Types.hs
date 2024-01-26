{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Minichain.ChatModels.Types ( Minichain 
                                  , ApiKey 
                                  , Role(..)
                                  , Message(..), role, content
                                  , pattern UserMessage 
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

type Minichain model = StateT model IO

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

-- | A class for Chat Models
class ChatModel a where 
    -- | Predict for current and only model
    predict :: (MonadIO m, MonadThrow m) => String -> StateT a m [Message]
    predict = predicts id

    -- | Predict for a specific model via lens
    predicts :: (MonadIO m, MonadThrow m) => Lens' s a -> String -> StateT s m [Message]
    predicts f = predictsMsg f . UserMessage

    -- | Predict Message type for current and only model
    predictMsg :: (MonadIO m, MonadThrow m) => Message -> StateT a m [Message]
    predictMsg = predictsMsg id

    -- | Predict Message type for specific model
    predictsMsg :: (MonadIO m, MonadThrow m) => Lens' s a -> Message -> StateT s m [Message]
    predictsMsg f msg = predictsMsgs f [msg]

    -- | Predict Multiple Message types for current and only model
    predictMsgs :: (MonadIO m, MonadThrow m) => [Message] -> StateT a m [Message]
    predictMsgs = predictsMsgs id

    -- | Predict Multiple Message types for specific model
    predictsMsgs :: (MonadIO m, MonadThrow m) => Lens' s a -> [Message] -> StateT s m [Message]

    -- | Add Messages for current and only model
    addMsgs :: Monad m => [Message] -> StateT a m ()
    addMsgs = addMsgsTo id

    -- | Add Messages for specific model
    addMsgsTo :: Monad m => Lens' s a -> [Message] -> StateT s m ()
