{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module DotEnv (DotEnv, EnvPath(..), loadEnv, getEnv) where

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap

-- | A DotEnv is a HashMap of ByteString keys and values
type DotEnv = HashMap ByteString ByteString

-- | An EnvPath is either a path to a .env file or the default .env file
data EnvPath = EnvPath FilePath | DefaultEnv

-- | Convert an EnvPath to a FilePath
fromEnvPath :: EnvPath -> FilePath 
fromEnvPath (EnvPath path) = path 
fromEnvPath DefaultEnv = ".env"

-- | Load a DotEnv from an EnvPath
loadEnv :: EnvPath -> IO DotEnv
loadEnv path = HashMap.fromList . map parseEnvLine . BS.lines <$> BS.readFile (fromEnvPath path)

-- | Get a value from a DotEnv
--
-- Format: key=value
getEnv :: DotEnv -> ByteString -> Maybe ByteString
getEnv = flip HashMap.lookup

-- | Parse a line from a .env file
parseEnvLine :: ByteString -> (ByteString, ByteString)
parseEnvLine x = let (key, BS.tail -> value) = BS.break (== '=') x in (key, value)
