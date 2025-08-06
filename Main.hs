{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Text.IO as T
import Options.Generic
import System.Exit
import System.IO

import Llama

data Options w = Options
  { systemPrompt :: w ::: Text <?> "The system prompt to use" <!> "You are a helpful assistant."
  , url :: w ::: String <?> "llama-server URL" <!> "http://localhost:8080"
  } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  opts <- unwrapRecord "A command line interface for llama-server"
  input <- T.getContents
  let request = [ LlamaMessage System $ systemPrompt opts
                , LlamaMessage User input
                ]
  response <- llamaTemplated (url opts) (LlamaApplyTemplateRequest request)
  case response of
    Nothing -> T.hPutStrLn stderr "Got no response from the server." >> exitFailure
    Just r -> T.putStrLn r
