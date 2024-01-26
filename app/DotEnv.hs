{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module DotEnv (DotEnv, EnvPath(..), loadEnv, getEnv) where

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap

type DotEnv = HashMap ByteString ByteString

data EnvPath = EnvPath FilePath | DefaultEnv

fromEnvPath :: EnvPath -> FilePath 
fromEnvPath (EnvPath path) = path 
fromEnvPath DefaultEnv = ".env"

loadEnv :: EnvPath -> IO DotEnv
loadEnv path = HashMap.fromList . map parseEnvLine . BS.lines <$> BS.readFile (fromEnvPath path)

getEnv :: DotEnv -> ByteString -> Maybe ByteString
getEnv = flip HashMap.lookup

parseEnvLine :: ByteString -> (ByteString, ByteString)
parseEnvLine x = let (key, BS.tail -> value) = BS.break (== '=') x in (key, value)
