{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeChain.ChatModels.PromptTemplate (makeTemplate, user, assistant, system) where

import Data.List (nub)

import Language.Haskell.TH

import TypeChain.ChatModels.Types

data TemplateToken = ConstString String | Var String deriving Eq

type PromptTemplate = (Q Exp, [Name])

user :: String -> Q PromptTemplate
user xs = toTemplate xs [| UserMessage |]

assistant :: String -> Q PromptTemplate
assistant xs = toTemplate xs [| AssistantMessage |]

system :: String -> Q PromptTemplate
system xs = toTemplate xs [| SystemMessage |]

toTemplate :: String -> Q Exp -> Q PromptTemplate
toTemplate xs f = do 
    let tokens = parseTemplateTokens xs
        names  = map mkName $ nub $ getVarTokens tokens
        params = map varP names
        func   = lamE params (appE f $ tokensToExpr tokens)
        expr   = foldl appE func (map varE names)

    return (expr, names)

makeTemplate :: [Q PromptTemplate] -> Q Exp
makeTemplate xs = do 
    (exps, ps) <- unzip <$> sequence xs
    let params = nub $ concat ps
    
    lamE (map varP params) $ listE exps

parseTemplateTokens :: String -> [TemplateToken]
parseTemplateTokens [] = [] 
parseTemplateTokens ('{':xs) = Var first : parseTemplateTokens rest
    where (first, tail -> rest) = break (== '}') xs
parseTemplateTokens (x:xs) = ConstString (x : first) : parseTemplateTokens rest
    where (first, rest) = break (== '{') xs

getVarTokens :: [TemplateToken] -> [String]
getVarTokens [] = [] 
getVarTokens (Var x : xs) = x : getVarTokens xs
getVarTokens (_     : xs) = getVarTokens xs

tokensToExpr :: [TemplateToken] -> Q Exp
tokensToExpr [] = [| "" |]
tokensToExpr (ConstString x : xs) = [| x ++ $(tokensToExpr xs) |]
tokensToExpr (Var x : xs) = appE [| (++) |] (varE (mkName x)) `appE` tokensToExpr xs
