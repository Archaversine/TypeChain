{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module TypeChain.ChatModels.Prompts (makeTemplate) where

import Data.List (nub)

import Language.Haskell.TH

data TemplateToken = ConstString String | Var String deriving Eq

-- | Template Function Examples:
-- input: @"Your name is {name}."@  
-- output: @\name -> "Your name is " ++ name ++ "."@
--
-- input: @"Your name is {name} and you are {age} years old."@
-- output: @\name age -> "Your name is " ++ name ++ " and you are " ++ age ++ " years old."@
makeTemplate :: String -> Q Exp
makeTemplate str = do 
    let tokens = parseTemplateTokens str
        names  = nub $ getVarTokens tokens

    lamE (map (varP . mkName) names) (tokensToExpr tokens)

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
    where name = mkName x

