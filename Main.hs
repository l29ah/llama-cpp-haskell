{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Conduit
import Data.Conduit.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Generic
import System.Exit
import System.IO

import Llama
import Llama.Streaming as LS

data Options w = Options
  { systemPrompt :: w ::: Text <?> "The system prompt to use" <!> "You are a helpful assistant."
  , url :: w ::: String <?> "llama-server URL" <!> "http://localhost:8080"
  , streaming :: w ::: Bool <?> "use to stream output from the LLM"
  , stripThinking :: w ::: Bool <?> "remove \"</think>\" and everything that occurs before it, only works in non-streaming mode"
  } deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  opts <- unwrapRecord "A command line interface for llama-server"
  input <- T.getContents
  let request = [ LlamaMessage System $ systemPrompt opts
                , LlamaMessage User input
                ]
  case streaming opts of
    False -> do
      response <- llamaTemplated (url opts) (LlamaApplyTemplateRequest request)
      case response of
        Nothing -> T.hPutStrLn stderr "Got no response from the server." >> exitFailure
        Just r -> T.putStrLn $ if stripThinking opts then snd $ T.breakOnEnd "</think>" r else r
    True -> do
      hSetBuffering stdout NoBuffering
      conduit <- llamaTemplatedStreaming (url opts) (LlamaApplyTemplateRequest request)
      runResourceT $ do
        list <- lazyConsume conduit
        liftIO $ mapM_ (T.putStr . LS.content) $ list
      T.putStrLn ""
