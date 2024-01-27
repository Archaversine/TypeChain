{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeChain.ChatModels.Prompts (makeTemplate, user, assistant, system) where

import Data.List (nub)

import Language.Haskell.TH

import TypeChain.ChatModels.Types

data TemplateToken = ConstString String | Var String deriving Eq

user :: Q Exp 
user = [| UserMessage |]

assistant :: Q Exp 
assistant = [| AssistantMessage |] 

system :: Q Exp 
system = [| SystemMessage |]

makeTemplate :: [(Q Exp, String)] -> Q Exp
makeTemplate xs = do 
    (exps, ps) <- mapAndUnzipM toTemplate xs 
    let params = nub $ concat ps
    
    lamE (map varP params) $ listE exps

toTemplate :: (Q Exp, String) -> Q (Q Exp, [Name]) 
toTemplate (f, str) = do 
    let tokens = parseTemplateTokens str
        names  = map mkName $ nub $ getVarTokens tokens
        params = map varP names
        func   = lamE params (appE f $ tokensToExpr tokens)
        expr   = foldl appE func (map varE names)

    return (expr, names)

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
