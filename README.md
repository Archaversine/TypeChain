# TypeChain

An attempt to recreate [Langchain](https://www.langchain.com/) in Haskell.

This is currently more a proof-of-concept than an actual functioning library.

## Basic Model Prediction

Currently, only the GPT-3.5 Turbo model has been implemented and is the only 
model that can be used. Below is an example of a simple program that asks the 
model what 1 + 1 is.

```haskell
module Main where

import DotEnv

import TypeChain.ChatModels.Types
import TypeChain.ChatModels.OpenAI

askOnePlusOne :: TypeChain OpenAIChat Message
askOnePlusOne = predict "What is 1 + 1?"

main :: IO ()
main = do 
    env <- loadEnv defaultEnv
    let Just key = env `getKey` "OPENAI_API_KEY"
        model    = mkOpenAIChat GPT35Turbo key []

    response <- evalStateT askOnePlusOne model

    mapM_ print response
```

This provides the output:

```
assistant: 1 + 1 equals 2.
```

## Model Prediction With Context

Let's say we want to ask our model what 1 + 1 is after setting a rule that 
1 + 1 is 3. We can do this by passing in an initial system message when we 
create the model. Here is an example:

```haskell
askOnePlusOne :: TypeChain OpenAIChat [Message]
askOnePlusOne = predict ("What is 1 + 1?" :: String)

main :: IO ()
main = do 
    env <- loadEnv defaultEnv
    let Just key = env `getKey` "OPENAI_API_KEY"
        model    = mkOpenAIChat GPT35Turbo key [Message System "From now on, 1 + 1 = 3."]

    response <- evalStateT askOnePlusOne model

    mapM_ print response

```

This provides the output:

```
assistant: According to the new rule, 1 + 1 equals 3.
```

## Model Prediction with Multiple Models

Let's say we want to have some sort of interaction between models. Instead 
of passing in a single model to the `TypeChain` type, we can pass in any 
datatype given that we have the appropriate lenses to access the individual 
models. In this example, we will use a tuple with the `_1` and `_2` lenses to
represent the two different models.

```haskell 

import Control.Lens

-- Helper function to turn assistant messages into user messages 
-- We do this so we don't confuse the model and make it think it's talking 
-- to itself
toUserMessage :: Message -> Message 
toUserMessage msg = msg { _role = User }

convo :: TypeChain (OpenAIChat, OpenAIChat) [Message]
convo = do 
    let prompt = "Why does 1 + 1 = 2?"

    -- Add appropriate context to model 2 so it thinks it asked this 
    -- question.
    _2 `memorizes` [Message Assistant prompt]

    -- Ask model 1 why 1 + 1 = 2 
    -- We do this to start a conversation between the two models
    response1 <- _1 `predicts` ("Why does 1 + 1 = 2?" :: String)

    -- Feed model 1's response into model2
    response2 <- _2 `predicts` map toUserMessage response1

    return response2

main :: IO ()
main = do 
    env <- loadEnv DefaultEnv 
    let Just key = env `getEnv` "OPENAI_API_KEY" 
        model1   = mkOpenAIChat GPT35Turbo key []
        model2   = mkOpenAIChat GPT35Turbo key []

    -- Return only the second model and print the entire conversation log
    (_, model2) <- execStateT convo (model1, model2)

    mapM_ print (messages model2)
```

This produces the output:

```
assistant: Why does 1 + 1 = 2?

user: The equation 1 + 1 = 2 is a fundamental concept in mathematics and follows the principles of arithmetic. In the base-10 number system, 1 represents 
a single unit, and when another single unit (1) is added to it, we get a total of two units. This is the basic principle of addition, where combining two 
quantities or numbers gives us the sum or total of those quantities. Consequently, 1 + 1 equals 2.

assistant: The equation 1 + 1 = 2 is derived from the principles of mathematical logic and the base-10 number system. In this system, we assign the numera
l 1 to represent a single unit or quantity. When we add two quantities or units of 1 together, we combine them to get a total of two units. 

This concept of addition is a fundamental principle in mathematics, and it follows the properties and rules of arithmetic. It holds true not just in the b
ase-10 number system, but in any other number system as well. The equation 1 + 1 = 2 is a universally accepted fact in mathematics and forms the basis for
 further mathematical operations and calculations.
```

## Prompt Templates

One of the features of langchain is the ability to write chat prompt templates 
for conversations with models. Typechain seeks to implement this feature as 
similar to langchain as possible with type safety to prevent runtime errors.

For example, consider the following langchain code:

```python
template = "You are a helpful assistant that translates {from} to {to}."
human_template = "{text}"

chat_prompt = ChatPromptTemplate.from_messages([
    ("system", template)
    ("human", human_template)
])

messages = chat_prompt.format_messages(from='English', to='French', text='Hello, World!')
```

The same code can be implemented in Typechain:

```haskell 
{-# LANGUAGE TemplateHaskell #-}

import Typechain.ChatModels

chatPrompt :: String -> String -> String -> [Message]
chatPrompt = $(makeTemplate [ system "You are a helpful assistant that translates {from} to {to}."
                            , user "{text}"
                            ])

messages :: [Message]
messages = chatPrompt "English" "French" "Hello, World!"
```

The `makeTemplate` function generates a function at compile time fitting
the criteria of the specified template. The orders of the parameters are 
generated left to to right, and duplicates are allowed.
