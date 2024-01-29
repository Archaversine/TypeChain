{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TypeChain.ChatModels.PromptTemplate (ToPrompt(..), makeTemplate, user, assistant, system) where

import Data.Char (toLower)
import Data.List (nub)

import Language.Haskell.TH

import TypeChain.ChatModels.Types

data TemplateToken = ConstString String | Var String deriving Eq

type PromptTemplate = (Q Exp, [Name])

class ToPrompt a where 
    toPrompt :: a -> [Message]

user :: String -> Q PromptTemplate
user xs = toTemplate xs [| UserMessage |]

assistant :: String -> Q PromptTemplate
assistant xs = toTemplate xs [| AssistantMessage |]

system :: String -> Q PromptTemplate
system xs = toTemplate xs [| SystemMessage |]

toTemplate :: String -> Q Exp -> Q PromptTemplate
toTemplate xs f = do 
    let tempParam = mkName "template"

    let tokens = parseTemplateTokens xs
        expr   = tokensToExpr tempParam tokens
        names  = map mkName $ nub $ getVarTokens tokens

    return (appE f expr, names)

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

tokensToExpr :: Name -> [TemplateToken] -> Q Exp
tokensToExpr _ [] = [| "" |]
tokensToExpr name (ConstString x : xs) = [| x ++ $(tokensToExpr name xs) |]
tokensToExpr name (Var x : xs) = appE [| (++) |] (appE (varE $ mkName x) (varE name)) `appE` tokensToExpr name xs


-- | Given a typename and a list of messages, generate a data type and a function to construct it.
--
-- Example: `makeTemplate "Translate" [system "translate {a} to {b}.", user "{text}"]`
--
-- This generates a record named @Translate@ with fields @a@, @b@, and @text@. 
-- It also generates a function @mkTranslate :: String -> String -> String -> [Message]@.
-- To allow for quick and easy construction of the prompt if needed. Otherwise, you can use the 
-- generated data type in conjunction with the `toPrompt` function to be more explicit.
makeTemplate :: String -> [Q PromptTemplate] -> Q [Dec] 
makeTemplate name xs = do 
    let typeName = mkName name
        funcName = mkName ("mk" ++ name)

    (exps, concat -> nub -> names) <- unzip <$> sequence xs

    exps'   <- sequence exps
    varbang <- bang sourceNoUnpack sourceStrict

    let recordFields      = map (, varbang, ConT ''String) names :: [VarBangType]
        promptFunc        = FunD 'toPrompt [Clause [VarP $ mkName "template"] (NormalB $ ListE exps') []]
        filledConstructor = foldl AppE (ConE typeName) (map VarE names)
        mkFuncClause      = Clause (map VarP names) (NormalB $ AppE (VarE 'toPrompt) filledConstructor) []

    return [ DataD     [] typeName [] Nothing [RecC typeName recordFields] []
           , InstanceD Nothing [] (AppT (ConT ''ToPrompt) (ConT typeName)) [promptFunc]
           , FunD      funcName [mkFuncClause]
           ]
